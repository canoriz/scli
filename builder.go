package scli

import (
	"errors"
	"fmt"
	"os"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"sync/atomic"
	"unsafe"
)

// Parse is the interface for custom type. Any type implemented this interface
// can be used as an argument/option in scli's definition struct.
type Parse interface {
	FromString(s string) error
	Example() string
}

// ParseOnce is an interface for types that are Parse, but the FromString() of
// the type should be called at most once. This restriction is vital for
// implementing types like ConfigFile. Lets see why. First, some definitions.
//
// Definitions:
//
// 1. Stateful
// Assuming all the outside running environment is unchanged, for instance t,
// calling t.FromString() once and calling t.FromString(..) for the second time
// will result in different behavior. One example is FromString contains logics
// based on a global variable which is inherited next time, then the second call
// will not trigger the same logic.
//
// 2. Impure
// For instance t, the result of t.FromString(..) not only dependent on the
// code of FromString it self, and may produce different outcome based on the
// running environment. One example is FromString read a file, and file may or
// may not exist on different environment.
//
// For ConfigFile type, apparently, it's Impure because it relies on files which
// is not part of the code.
// For ConfigFile that can live update it's value when file is changed, the type
// is Stateful. A watcher is created for the first time a FromString() success
// and when file changes, FromString is called again to load new data, but not
// creating new watchers.
//
// For ParseOnce, scli will not check default value nor example value, and
// Parser.Parse()FromString is called only once at parsing.
// If you think your type T is ParseOnce, use MarkOnce[T] to convert your Parse
// type to ParseOnce
type ParseOnce interface {
	Parse

	// make it private so users cannot implement their own ParseOnce
	statefulOrImpure()
}

// EXPERIMENTAL: MarkOnce[T]: convert Parse to ParseOnce
// for type T that *T is Parse
//
//	struct {
//		t T
//	}
//
// should now define:
//
//	type TOnce = scli.MarkOnce[*T]
//
//	struct {
//			tOnce TOnce
//	}
//
// and get T by call tOnce.Get()
type MarkOnce[T Parse] struct {
	called atomic.Bool
	t      T
}

func (m *MarkOnce[T]) FromString(s string) error {
	if m.called.CompareAndSwap(false, true) {
		valTy := reflect.TypeOf(m.t).Elem()
		tField := reflect.ValueOf(m).Elem().Field(1)

		// convert unexported field to a reflect.Value that can set
		tField = reflect.NewAt(tField.Type(), unsafe.Pointer(tField.UnsafeAddr())).Elem()
		tField.Set(reflect.New(valTy))
		return m.t.FromString(s)
	}
	panic("FromString() of ParseOnce is called twice")
}

func (m *MarkOnce[T]) Example() string {
	return m.t.Example()
}

func (m *MarkOnce[T]) Get() *T {
	return &m.t
}

func (m *MarkOnce[T]) statefulOrImpure() {}

var (
	ErrIsHelp = errors.New("argument is help message")
)

const ( // build time errors
	errParseDefault      = `error parsing default value "%s" of field "%s", error: %w`
	errHelpIsReserved    = `"help" is a reserved word, please change option name of "%s"`
	errNotImplParse      = "*%s did not implement `Parse` interface, at field %s"
	errCmdRedefined      = `subcommand %s is redefined, at field %s`
	errOptionRedefined   = `option %s is redefined, at field %s`
	errArgRedefined      = `argument %s is redefined, at field %s`
	errBothArgAndCommand = `command %s and argument %s both defined, which is not allowed,` +
		` at field %s and %s`
	errNotStructPtr = "type %v of field %s must be *struct{...} to represent a subcommand, " +
		"or type `Parse` to represent a custom type"
	errNotValidExample = "Example() of custom type *%s cannot parsed by FromString(..) " +
		"at field %s. If you are sure parsing Example() may fail and it is AS EXPECTED, " +
		"consider use MarkOnce, see MarkOnce"
	errArgNoDefault = "positional argument type %s at field %s requires a default value " +
		"because a previous argument has default value"
	errSliceArgHasDefault = "slice argument cannot have default value: argument %s at field %s"
	errMultipleArgSlice   = `more than one slice type argument: %s at field %s`
	errArgAfterSlice      = `cannot define argument after slice argument: %s at field %s`

	errSliceArgAfterDefaultArg = "cannot define slice argument after optional(has default)" +
		" argument: %s at field %s"
)

const ( // runtime errors
	errOptionNoValue      = `no value provided for option "--%s" in %s`
	errOptionNotFound     = `option "--%s" is required in "%s" but not provided`
	errOptionDuplicate    = `option "--%s" is given more than once`
	errNoOption           = `option "--%s" provided in "%s" but not defined`
	errNoSubcommand       = `subcommand "%s" provided in "%s" but not defined`
	errRemainUnparsed     = `parse success until "%s", maybe remove "%s" and try again?`
	errParseOption        = `error parsing argument of option "--%s %s" error: %w`
	errParsePositionalArg = `error parsing positional argument "%s" value %s, error: %w`
	errTooManyArgs        = `error parsing positional argument: too many args, expect %d, given %d args`
	errArgNotFound        = `argument <%s> is required in "%s" but not provided`
)

func maxInt(a, b int) int {
	if a < b {
		return b
	}
	return a
}

// type inputType int
// const (
// 		notOption inputType = iota
// 		validOption
// 		maybeOption

type parseCmdResult struct {
	rv        reflect.Value
	isHelpCmd bool
	helpText  string
}

type parseArgResult struct {
	rv reflect.Value
}

func (p parseCmdResult) IsHelp() bool {
	return p.isHelpCmd
}

type parseCmdError struct {
	// err is the error causing parse failed
	// usage is the correct usage, will be printed when
	// error happens
	err   error
	usage string
}

func (pe *parseCmdError) Error() string {
	return pe.err.Error()
}

type parseCmdFn func([]string) (parseCmdResult, *parseCmdError)

// TODO: parseArgResult is not used now. The set work is now done
// by parse function it self. The parseArgResult.rv is not used anymore.
// Maybe change signature return error only
type parseArgFn func([]string) (parseArgResult, error)

type Parser[T any] struct {
	parse parseCmdFn
	help  string

	checkFn func(T) error
}

func (p Parser[T]) Checker(checkFn func(T) error) Parser[T] {
	p.checkFn = checkFn
	return p
}

func (p Parser[T]) ParseArg(s string) (zero T, err error) {
	r, e := p.parse(strings.Fields(s))
	if e != nil {
		return zero, e
	}
	if r.IsHelp() {
		return zero, ErrIsHelp
	}
	res := r.rv.Interface().(T)
	if p.checkFn != nil {
		return res, p.checkFn(res)
	}
	return res, nil
}

func (p Parser[T]) Parse() T {
	r, e := p.parse(os.Args[1:])
	if e != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", e)
		fmt.Println()
		fmt.Println("The usage is:")
		fmt.Print(e.usage)
		os.Exit(2)
	}
	if r.IsHelp() {
		fmt.Print(r.helpText)
		os.Exit(0)
	}
	res := r.rv.Interface().(T)
	if p.checkFn != nil {
		if err := p.checkFn(res); err != nil {
			fmt.Fprintf(os.Stderr, "parse ok, but check failed: %v\n", err)
			fmt.Println()
			fmt.Println("The usage is:")
			fmt.Print(r.helpText)
			os.Exit(2)
		}
	}
	return res
}

func (p Parser[T]) Help() string {
	return p.help
}

func BuildParser[T any](u *T) Parser[T] {
	parse, topHelp := checkTopAndBuildParseFn(u, os.Args[0])
	return Parser[T]{parse: parse, help: topHelp}
}

func checkTopAndBuildParseFn(u any, execName string) (parseCmdFn, string) {
	if err := checkType(u); err != nil {
		panic(err)
	}
	return buildParseCmdFn(
		reflect.ValueOf(u).Elem().Type().Name(),
		execName,
		reflect.ValueOf(u),
	)
}

func checkType(u any) error {
	ptr := reflect.ValueOf(u) // any -> *T
	if ptr.Kind() != reflect.Pointer || ptr.IsNil() {
		return errors.New("in BuildParser(v), type of v must be non nil *struct{...}")
	}
	argStructPtr := ptr.Elem() // *T -> T
	if argStructPtr.Kind() != reflect.Struct {
		return errors.New("in BuildParser(v), type of v must be *struct{...}")
	}
	return nil
}

const (
	// TODO: use map[kwType]?
	boolArgLen    = 1
	argLen        = 2
	subcommandLen = 1
)

type kwType int

const (
	boolArg kwType = iota
	valArg
	sliceArg
)

// cmdInfo contains general information of a command
// A command can have options, arguments and sub-commands
// cmdInfo is used to build parse functions
type cmdInfo struct {
	options map[string]optionInfo
	subcmds map[string]subcmdInfo
	args    []argInfo // preserving order of positional arguments
}

func newCmdInfo() cmdInfo {
	return cmdInfo{
		options: map[string]optionInfo{},
		subcmds: map[string]subcmdInfo{},
		args:    []argInfo{},
	}
}

// optionInfo contains information for options (--option value)
type optionInfo struct {
	parseFn    parseArgFn
	defName    string // name of field in struct definition
	consumeLen int
	ty         kwType // type of option argument. boolean or normal argument
	usage      string

	defaultVal *string // non-nil if argument has a default value
	example    *string // non-nil if argument is a custom type

	// the details of Parse interface if this is a custom type
	parseDetail parseStatus
}

// argInfo contains information for positional arguments
// eg: cmd arg1 arg2
type argInfo struct {
	cliName string // argument name in CLI
	parseFn parseArgFn
	defName string // name of field in struct definition
	ty      kwType // type of argument
	usage   string

	defaultVal *string // non-nil if argument has a default value
	example    *string // non-nil if argument is a custom type

	// the details of Parse interface if this is a custom type
	parseDetail parseStatus
}

// define argOrOption to make call simpler
type argOrOption struct {
	isArg bool

	// common for both argument and option
	parseFn parseArgFn
	defName string // name of field in struct definition
	usage   string
	example *string // non-nil if custom type

	// only valid if isArg == false, e.g. is option
	consumeLen int
	ty         kwType  // type of option argument. boolean or normal argument
	defaultVal *string // non-nil if argument has a default value

	// the details of Parse interface if this is a custom type
	parseDetail parseStatus
}

// subcmdInfo contains information of a subcommand
type subcmdInfo struct {
	parseFn parseCmdFn
	defName string
	usage   string
}

func buildParseCmdFn(
	fieldChain string, viewNameChain string, u reflect.Value,
) (p parseCmdFn, usageText string) {
	argStructPtr := u.Elem() // *T -> T
	structDef := u.Type().Elem()

	cmd, err := buildArgAndCommandList(
		fieldChain, viewNameChain, structDef, argStructPtr,
	)
	if err != nil {
		panic(err)
	}
	err = validateArgAndCommand(fieldChain, cmd)
	if err != nil {
		panic(err)
	}

	// parseOption try to parse input as an option
	// if ok, set argStruct and returns (remaining input, nil)
	// if failed, returns (input unchanged, error)
	parseOption := func(cmd cmdInfo, helpText string, input []string) (
		_rest []string, _err *parseCmdError,
	) {
		first, _ := input[0], input[1:]
		option := strings.Trim(first, "-/")

		t, ok := cmd.options[option]
		if !ok {
			return input, &parseCmdError{
				err:   fmt.Errorf(errNoOption, option, viewNameChain),
				usage: helpText,
			}
		}
		if t.ty == boolArg {
			// bool is special, no need to parse, set value now
			argStructPtr.FieldByName(t.defName).Set(
				reflect.ValueOf(
					!strings.HasPrefix(strings.Trim(first, "-"), "/"),
				),
			)
		} else {
			if len(input) < t.consumeLen {
				return input, &parseCmdError{
					err: fmt.Errorf(
						errOptionNoValue, option, viewNameChain,
					),
					usage: helpText,
				}
			}
			val := input[1:t.consumeLen] // ["--option", "arg"], we take "arg"
			_, err := t.parseFn(val)
			if err != nil {
				return input, &parseCmdError{
					err: fmt.Errorf(
						errParseOption,
						option, strings.Join(val, ""), err,
					),
					usage: helpText,
				}
			}
		}
		return input[t.consumeLen:], nil
	}

	// parseArg try to parse input as an argument
	// if ok, set argStruct and returns (remaind input, nil)
	// if failed, returns (input unchanged, error)
	parseArg := func(
		cmd cmdInfo, helpText string,
		argNumber int, input []string,
	) (
		_rest []string, _err *parseCmdError,
	) {
		first, _ := input[0], input[1:]
		if len(cmd.args) <= argNumber {
			return input, &parseCmdError{
				err: fmt.Errorf(
					errTooManyArgs,
					len(cmd.args),
					len(input)+len(cmd.args),
				),
				usage: helpText,
			}
		}

		t := cmd.args[argNumber]
		if t.ty == sliceArg {
			_, err := t.parseFn(input)
			if err != nil {
				return input, &parseCmdError{
					err: fmt.Errorf(
						errParsePositionalArg,
						t.cliName, strings.Join(input, " "), err,
					),
					usage: helpText,
				}
			}
			return []string{}, nil // slice arg consumes all input
		}

		// argument is not slice
		_, err := t.parseFn([]string{first})
		if err != nil {
			return input, &parseCmdError{
				err: fmt.Errorf(
					errParsePositionalArg,
					t.cliName, first, err,
				),
				usage: helpText,
			}
		}
		return input[1:], nil
	}

	// parseSubcmd try to parse input as an subcommand
	// If ok, set argStruct and returns (_, parseCmdResult, nil)
	// If failed, returns (_, nil, error)
	// If input is a subcommand help or subcommand error, directReturn will be true
	// caller should return immediately
	parseSubcmd := func(cmd cmdInfo, helpText string, input []string) (
		_directReturn bool, _result parseCmdResult, _err *parseCmdError,
	) {
		first, rest := input[0], input[1:]
		t, ok := cmd.subcmds[first]
		if !ok {
			return true, parseCmdResult{}, &parseCmdError{
				err: fmt.Errorf(
					errNoSubcommand, first, viewNameChain,
				),
				usage: helpText,
			}
		}
		r, err := t.parseFn(rest)
		if err != nil {
			return true, r, err
		}
		if r.IsHelp() {
			return true, r, nil
		}
		argStructPtr.FieldByName(t.defName).Set(r.rv.Addr())
		return false, parseCmdResult{}, nil
	}

	parse := func(input []string) (parseCmdResult, *parseCmdError) {
		encounter := make(map[string]bool)
		argsProcessed := 0
		helpText := makeUsageText(viewNameChain, cmd)

		noMoreOption := false
		// parse options and args
		for len(input) > 0 {
			first, _ := input[0], input[1:]
			// if input starts with "-" but not valid option,
			// it may be a argument value
			if strings.HasPrefix(first, "-") && !noMoreOption {
				realOption := strings.Trim(first, "-/")
				if realOption == "help" {
					return parseCmdResult{
						helpText:  helpText,
						isHelpCmd: true,
					}, nil
				}
				if _, ok := encounter[realOption]; ok {
					// encoutered before
					return parseCmdResult{}, &parseCmdError{
						err:   fmt.Errorf(errOptionDuplicate, realOption),
						usage: helpText,
					}
				}

				remain, parseOptionErr := parseOption(cmd, helpText, input)
				if parseOptionErr != nil {
					remain, parseArgErr := parseArg(cmd, helpText, argsProcessed, input)
					if parseArgErr != nil {
						// TODO: tell user it's not option and also not arg
						return parseCmdResult{}, parseOptionErr
					}
					// the input parsed success, from now on, no more options
					// all input should be args
					noMoreOption = true
					input = remain
					argsProcessed++
				} else {
					encounter[realOption] = true
					input = remain
				}
			} else if cmd.HasArgs() {
				if len(cmd.args) <= argsProcessed {
					return parseCmdResult{}, &parseCmdError{
						err: fmt.Errorf(
							errTooManyArgs,
							len(cmd.args),
							len(input)+len(cmd.args),
						),
						usage: helpText,
					}
				}
				noMoreOption = true
				remain, err := parseArg(cmd, helpText, argsProcessed, input)
				if err != nil {
					return parseCmdResult{}, err
				}
				input = remain
				argsProcessed++
			} else {
				break
			}
		}

		if len(input) > 0 {
			if cmd.HasSubcmds() {
				directReturn, r, err := parseSubcmd(cmd, helpText, input)
				if directReturn {
					return r, err
				}
				if err != nil {
					return parseCmdResult{}, err
				}
			} else {
				// if input remains and no available sub commands, it's a error
				return parseCmdResult{}, &parseCmdError{
					err: fmt.Errorf(
						errRemainUnparsed,
						strings.Join(input, " "), strings.Join(input, " "),
					),
					usage: helpText,
				}
			}
		}

		// check all arguments are given
		// if struct did not define argument, cmd.args will be empty
		// below check will pass
		if cmd.HasArgs() {
			for _, arg := range cmd.args[argsProcessed:] {
				if arg.defaultVal != nil {
					_, err := arg.parseFn([]string{*arg.defaultVal})
					if err != nil {
						panic(
							"default values are validated before. " +
								"Cannot parse error at parse",
						)
					}
				} else if arg.ty == sliceArg {
					// sliceArg can be omitted to represent empty slice
				} else {
					return parseCmdResult{}, &parseCmdError{
						err: fmt.Errorf(
							errArgNotFound,
							arg.cliName, viewNameChain,
						),
						usage: helpText,
					}
				}
			}
		}

		// check all required options are given
		for optionName, info := range cmd.options {
			if _, argFound := encounter[optionName]; !argFound {
				if info.defaultVal != nil {
					_, err := info.parseFn([]string{*info.defaultVal})
					if err != nil {
						panic(
							"default values are validated before. " +
								"Cannot parse error at parse",
						)
					}
				} else {
					return parseCmdResult{}, &parseCmdError{
						err: fmt.Errorf(
							errOptionNotFound,
							optionName,
							viewNameChain,
						),
						usage: helpText,
					}
				}
			}
		}

		return parseCmdResult{
			rv:       argStructPtr,
			helpText: helpText,
		}, nil
	}
	return parse, makeUsageText(viewNameChain, cmd)
}

type mapString[V any] struct {
	key string
	val V
}

func sortMap[V any](m map[string]V) []mapString[V] {
	r := make([]mapString[V], len(m))
	i := 0
	for k, v := range m {
		r[i] = mapString[V]{k, v}
		i++
	}
	sort.Slice(r, func(i, j int) bool {
		return r[i].key < r[j].key
	})
	return r
}

type cliInfo struct {
	options  map[string]optionInfo
	commands map[string]subcmdInfo
	args     []optionInfo
}

func buildArgAndCommandList(
	fieldChain string,
	viewNameChain string,
	structDef reflect.Type, argStructPtr reflect.Value,
) (cmdInfo, error) {
	// (options map[string]optionInfo, commands map[string]subcmdInfo, err error) {
	baseCmd := newCmdInfo()

	for i := 0; i < argStructPtr.NumField(); i++ {
		defField := structDef.Field(i)
		defName := defField.Name
		defaultVal := func(fieldDef reflect.StructField) *string {
			defaultValStr, hasDefault := fieldDef.Tag.Lookup("default")
			if hasDefault {
				return &defaultValStr
			}
			return nil
		}(defField)
		valField := argStructPtr.Field(i)
		currentFieldChain := fmt.Sprintf("%s.%s", fieldChain, defName)

		isArg, cliName := func() (bool, string) {
			optionName := defField.Tag.Get("flag") // for compatibility, use `flag`
			argName := defField.Tag.Get("arg")
			if argName == "" {
				argName = defName
			}
			if optionName != "" {
				return false, optionName
			}
			return true, argName
		}() // name used in cli

		usage := defField.Tag.Get("usage")

		p, parseDetail := hasCustomParser(valField)
		if parseDetail == exampleCannotParse {
			return baseCmd, fmt.Errorf(
				errNotValidExample,
				valField.Type(),
				currentFieldChain,
			)
		}
		if parseDetail == validParse || parseDetail == parseOnce {
			// has implemented Parse
			if err := baseCmd.AddArgOrOption(
				cliName,
				argOrOption{
					isArg:   isArg,
					defName: defName,

					parseFn:    p(valField),
					usage:      usage,
					example:    customExampleFor(valField),
					ty:         valArg,
					consumeLen: argLen,
					defaultVal: defaultVal,

					parseDetail: parseDetail,
				},
				currentFieldChain,
			); err != nil {
				return baseCmd, err
			}
		} else {
			switch {
			case valField.Kind() == reflect.Slice:
				ptrToElem := reflect.New(valField.Type().Elem())
				parseElemFn, elemExample, parseDetail, err :=
					func() (
						func(reflect.Value) parseArgFn,
						*string, // example
						parseStatus, // if type is custom, this contains the details
						error,
					) {
						p, parseDetail := hasCustomParser(
							reflect.New(ptrToElem.Type().Elem()).Elem(),
						)
						if parseDetail == exampleCannotParse {
							return nil, nil, exampleCannotParse,
								fmt.Errorf(
									errNotValidExample,
									ptrToElem.Type().Elem(),
									currentFieldChain,
								)
						}
						if parseDetail == validParse || parseDetail == parseOnce {
							// has implemented Parse
							return p, customExampleFor(
								reflect.New(ptrToElem.Type().Elem()).Elem(),
							), parseDetail, nil
						} else {
							switch ptrToElem.Elem().Type() {
							case reflect.TypeOf(string("")):
								strStr := "str"
								return parseString, &strStr, notParse, nil
							case reflect.TypeOf(bool(false)):
								falseStr := "false"
								return parseBool, &falseStr, notParse, nil
							case reflect.TypeOf(int(0)):
								intStr := "0"
								return parseInt, &intStr, notParse, nil
							case reflect.TypeOf(float64(0)):
								float64Str := "3.14"
								return parseFloat64, &float64Str, notParse, nil
							default:
								return nil, nil, notParse, fmt.Errorf(
									errNotImplParse,
									ptrToElem.Type(),
									currentFieldChain,
								)
							}
						}
					}()
				if err != nil {
					return baseCmd, err
				}

				parseFn, example := func() (parseArgFn, *string) {
					if isArg {
						parseFn := func(s []string) (parseArgResult, error) {
							// slice argument are passed separated by space
							if len(s) == 0 {
								return parseArgResult{
									rv: reflect.MakeSlice(valField.Type(), 0, 0),
								}, nil
							}
							valField.Set(reflect.MakeSlice(
								valField.Type(), len(s), len(s),
							))
							for i, si := range s {
								_, err := parseElemFn(valField.Index(i))([]string{si})
								if err != nil {
									return parseArgResult{}, err
								}
							}
							return parseArgResult{rv: valField}, nil
						}
						return parseFn, elemExample
					} else {
						parseFn := func(s []string) (parseArgResult, error) {
							str := strings.Join(s, "")
							if str == "" {
								return parseArgResult{
									rv: reflect.MakeSlice(valField.Type(), 0, 0),
								}, nil
							}
							strSlice := strings.Split(str, ",")
							valField.Set(reflect.MakeSlice(
								valField.Type(), len(strSlice), len(strSlice),
							))
							for i, si := range strSlice {
								// newElem := reflect.New(ptrToElem.Type().Elem()).Elem()
								_, err := parseElemFn(valField.Index(i))([]string{si})
								if err != nil {
									return parseArgResult{}, err
								}
							}
							return parseArgResult{rv: valField}, nil
						}
						example := func(elemExample *string) *string {
							if elemExample != nil {
								res := fmt.Sprintf(
									"%v,%v,...",
									*elemExample, *elemExample,
								)
								return &res
							}
							return nil
						}(elemExample)
						return parseFn, example
					}
				}()

				if err := baseCmd.AddArgOrOption(
					cliName,
					argOrOption{
						isArg:   isArg,
						defName: defName,

						parseFn:    parseFn,
						usage:      usage,
						example:    example,
						ty:         sliceArg,
						consumeLen: argLen, // only option use this length
						defaultVal: defaultVal,

						parseDetail: parseDetail,
					},
					currentFieldChain,
				); err != nil {
					return baseCmd, err
				}
			case valField.Type() == reflect.TypeOf(string("")):
				if err := baseCmd.AddArgOrOption(
					cliName,
					argOrOption{
						isArg:   isArg,
						defName: defName,

						parseFn:    parseString(valField),
						usage:      usage,
						ty:         valArg,
						consumeLen: argLen,
						defaultVal: defaultVal,
					},
					currentFieldChain,
				); err != nil {
					return baseCmd, err
				}
			case valField.Type() == reflect.TypeOf(bool(false)):
				if err := baseCmd.AddArgOrOption(
					cliName,
					argOrOption{
						isArg:   isArg,
						defName: defName,

						parseFn:    parseBool(valField),
						usage:      usage,
						ty:         boolArg,
						consumeLen: boolArgLen,
						defaultVal: defaultVal,
					},
					currentFieldChain,
				); err != nil {
					return baseCmd, err
				}
			case valField.Type() == reflect.TypeOf(int(0)):
				if err := baseCmd.AddArgOrOption(
					cliName,
					argOrOption{
						isArg:   isArg,
						defName: defName,

						parseFn:    parseInt(valField),
						usage:      usage,
						ty:         valArg,
						consumeLen: argLen,
						defaultVal: defaultVal,
					},
					currentFieldChain,
				); err != nil {
					return baseCmd, err
				}
			case valField.Type() == reflect.TypeOf(float64(0)):
				if err := baseCmd.AddArgOrOption(
					cliName,
					argOrOption{
						isArg:   isArg,
						defName: defName,

						parseFn:    parseFloat64(valField),
						usage:      usage,
						ty:         valArg,
						consumeLen: argLen,
						defaultVal: defaultVal,
					},
					currentFieldChain,
				); err != nil {
					return baseCmd, err
				}
			case valField.Kind() == reflect.Pointer: // expect a sub-command definition
				// should be *struct{...}
				fieldType := valField.Type().Elem() // reflect.Type struct{...}
				if fieldType.Kind() != reflect.Struct {
					return baseCmd, fmt.Errorf(
						errNotStructPtr,
						valField.Type(),
						currentFieldChain,
					)
				}

				instance := reflect.New(fieldType) // reflect.Value *struct{...}

				// usage text of subcommands is not used
				parseFn, _ := buildParseCmdFn(
					currentFieldChain,
					fmt.Sprintf("%s %s", viewNameChain, cliName),
					instance,
				)
				if err := baseCmd.AddCommand(
					cliName,
					subcmdInfo{
						parseFn: parseFn,
						defName: defName,
						usage:   usage,
					},
					currentFieldChain,
				); err != nil {
					return baseCmd, err
				}
			default:
				return baseCmd, fmt.Errorf(
					errNotImplParse,
					valField.Type(),
					currentFieldChain,
				)
			}
		}
	}
	return baseCmd, nil
}

// parseStatus is detailed status for Parse interface
type parseStatus int

const (
	notParse parseStatus = iota
	validParse
	exampleCannotParse
	parseOnce
)

// customParser return parseFn for this specific r: reflect.Value
// returns parseFn, exampleCannotParse, error
// if r is Parse, but Example() cannot Parse, return exampleCannotParse = true
func hasCustomParser(r reflect.Value) (
	_customParser func(typeSameAsR reflect.Value) parseArgFn,
	_status parseStatus,
) {
	if !r.CanAddr() {
		return nil, notParse
	}
	if parse, ok := r.Addr().Interface().(Parse); ok {
		_, isParseOnce := parse.(ParseOnce)
		if isParseOnce {
			return customParserFor, parseOnce
		}
		if !isParseOnce && parse.FromString(parse.Example()) != nil {
			return nil, exampleCannotParse
		}
		return customParserFor, validParse
	}
	return nil, notParse
}

// calling this function returns the specific function parse
// and set the reflect.Value r by the Parse method defined for
// type r.
// NOTICE: Caller MUST ensure r is *Parse and r.CanAddr()
func customParserFor(r reflect.Value) parseArgFn {
	return func(s []string) (parseArgResult, error) {
		e := r.Addr().
			Interface().(Parse).
			FromString(strings.Join(s, ""))
		return parseArgResult{
			rv: r, // r will be updated by FromString
		}, e
	}
}

// calling this function returns the specific .Example()
// of a reflect.Value r. Caller MUST ensure r is *Parse.
// and r.CanAddr()
func customExampleFor(r reflect.Value) *string {
	eg := r.Addr().
		Interface().(Parse).
		Example()
	return &eg
}

func validateArgAndCommand(fieldChain string, cmd cmdInfo) error {
	for optionName, info := range cmd.options {
		if optionName == "help" {
			return fmt.Errorf(
				errHelpIsReserved,
				fmt.Sprintf("%s.%s", fieldChain, info.defName),
			)
		}
		if info.defaultVal != nil && info.parseDetail != parseOnce {
			_, err := cmd.options[optionName].parseFn([]string{*info.defaultVal})
			if err != nil {
				return fmt.Errorf(
					errParseDefault,
					*info.defaultVal,
					fmt.Sprintf("%s.%s", fieldChain, info.defName),
					err,
				)
			}
		}
	}
	for _, arg := range cmd.args {
		if arg.defaultVal != nil && arg.parseDetail != parseOnce {
			_, err := arg.parseFn([]string{*arg.defaultVal})
			if err != nil {
				return fmt.Errorf(
					errParseDefault,
					*arg.defaultVal,
					fmt.Sprintf("%s.%s", fieldChain, arg.defName),
					err,
				)
			}
		}
	}
	return nil
}

func (c *cmdInfo) HasSubcmds() bool {
	return len(c.subcmds) > 0
}

func (c *cmdInfo) HasArgs() bool {
	return len(c.args) > 0
}

// Add argument or option
func (c *cmdInfo) AddArgOrOption(name string, a argOrOption, field string) error {
	if a.isArg {
		if err := c.AddArg(
			argInfo{
				parseFn: a.parseFn,
				cliName: name,
				defName: a.defName,
				usage:   a.usage,
				ty:      a.ty,

				example:    a.example,
				defaultVal: a.defaultVal,

				parseDetail: a.parseDetail,
			},
			field,
		); err != nil {
			return err
		}
		return nil
	}

	if err := c.addOption(
		name,
		optionInfo{
			parseFn:    a.parseFn,
			defName:    a.defName,
			consumeLen: a.consumeLen,
			ty:         a.ty,
			usage:      a.usage,

			defaultVal: a.defaultVal,
			example:    a.example,

			parseDetail: a.parseDetail,
		},
		field,
	); err != nil {
		return err
	}
	return nil
}

func (c *cmdInfo) addOption(
	optionName string,
	option optionInfo,
	field string, // original field's definition name
) error {
	if _, ok := c.options[optionName]; ok {
		return fmt.Errorf(errOptionRedefined, optionName, field)
	}
	c.options[optionName] = option
	return nil
}

func (c *cmdInfo) AddArg(
	arg argInfo,
	field string, // original field's definition name
) error {
	if c.HasSubcmds() {
		var cmdName string
		var cmdInfo subcmdInfo
		for cmdName, cmdInfo = range c.subcmds {
			// just get one subcommand
			break
		}
		return fmt.Errorf(
			errBothArgAndCommand,
			cmdName, arg.cliName,
			cmdInfo.defName, field,
		)
	}

	if arg.defaultVal != nil && arg.ty == sliceArg {
		return fmt.Errorf(errSliceArgHasDefault, arg.cliName, field)
	}

	for _, existArg := range c.args {
		if existArg.cliName == arg.cliName {
			return fmt.Errorf(errArgRedefined, arg.cliName, field)
		}
		if existArg.defaultVal != nil && arg.ty == sliceArg {
			return fmt.Errorf(errSliceArgAfterDefaultArg, arg.cliName, field)
		}
		if arg.ty == sliceArg && existArg.ty == sliceArg {
			return fmt.Errorf(errMultipleArgSlice, arg.cliName, field)
		}
		if arg.defaultVal == nil && existArg.defaultVal != nil {
			return fmt.Errorf(errArgNoDefault, arg.cliName, field)
		}
		if existArg.ty == sliceArg {
			return fmt.Errorf(errArgAfterSlice, arg.cliName, field)
		}
	}
	c.args = append(c.args, arg)
	return nil
}

func (c *cmdInfo) AddCommand(
	cmdName string,
	subcmd subcmdInfo,
	field string, // original field
) error {
	if c.HasArgs() {
		argName := c.args[0].cliName
		argDefName := c.args[0].defName
		return fmt.Errorf(
			errBothArgAndCommand,
			cmdName, argName,
			field, argDefName,
		)
	}
	if _, ok := c.subcmds[cmdName]; ok {
		return fmt.Errorf(
			errCmdRedefined,
			cmdName,
			field,
		)
	}
	c.subcmds[cmdName] = subcmd
	return nil
}

func parseBool(r reflect.Value) parseArgFn {
	return func(s []string) (parseArgResult, error) {
		b, e := strconv.ParseBool(strings.Join(s, ""))
		if e != nil {
			return parseArgResult{}, e
		}
		rv := reflect.ValueOf(b)
		r.Set(rv)
		return parseArgResult{rv: rv}, e
	}
}

func parseString(r reflect.Value) parseArgFn {
	return func(s []string) (parseArgResult, error) {
		sVal := strings.Join(s, "")
		rv := reflect.ValueOf(sVal)
		r.Set(rv)
		return parseArgResult{rv: rv}, nil
	}
}

func parseInt(r reflect.Value) parseArgFn {
	return func(s []string) (parseArgResult, error) {
		i, e := strconv.ParseInt(strings.Join(s, ""), 0, 64)
		if e != nil {
			return parseArgResult{}, e
		}
		rv := reflect.ValueOf(int(i))
		r.Set(rv)
		return parseArgResult{rv: rv}, e
	}
}

func parseFloat64(r reflect.Value) parseArgFn {
	return func(s []string) (parseArgResult, error) {
		f, e := strconv.ParseFloat(strings.Join(s, ""), 64)
		if e != nil {
			return parseArgResult{}, e
		}
		rv := reflect.ValueOf(f)
		r.Set(rv)
		return parseArgResult{rv: rv}, e
	}
}
