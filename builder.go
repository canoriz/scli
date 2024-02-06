package scli

import (
	"errors"
	"fmt"
	"os"
	"reflect"
	"sort"
	"strconv"
	"strings"
)

type Parse interface {
	FromString(s string) error
}

type Parser[T any] interface {
	Help() string
	Parse() T
	ParseArg(s string) (zero T, err error)
}

var (
	ErrIsHelp = errors.New("argument is help message")
)

const ( // build time errors
	errParseDefault   = `error parsing default value "%s" of field "%s", error: %w`
	errHelpIsReserved = `"help" is a reserved word, please change option name of "%s"`
	errNotImplParse   = "*%s did not implement `Parse` interface, at field %s"
	errNotStructPtr   = "type %v of field %s must be *struct{...} to represent a subcommand, " +
		"or type `Parse` to represent a custom type"
)

const ( // runtime errors
	errNoValue      = `no value provided for argument "--%s" in %s`
	errNoArgument   = `argument "--%s" provided in "%s" but not defined`
	errNoSubcommand = `subcommand "%s" provided in "%s" but not defined`
	errParseArg     = `error parsing argument "--%s %s" error: %w`
	errArgNotFound  = `argument "--%s" is required in "%s" but not provided`
)

func maxInt(a, b int) int {
	if a < b {
		return b
	}
	return a
}

type parseResult struct {
	rv        reflect.Value
	isHelpCmd bool
	helpText  string
}

func (p parseResult) IsHelp() bool {
	return p.isHelpCmd
}

type parseFn func([]string) (parseResult, error)

type parser[T any] struct {
	parse parseFn
	help  string
}

func (p parser[T]) ParseArg(s string) (zero T, err error) {
	r, e := p.parse(strings.Fields(s))
	if e != nil {
		return zero, e
	}
	if r.IsHelp() {
		return zero, ErrIsHelp
	}
	return r.rv.Interface().(T), nil
}

func (p parser[T]) Parse() T {
	r, e := p.parse(os.Args[1:])
	if e != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", e)
		fmt.Println()
		fmt.Println("The usage is:")
		fmt.Print(r.helpText)
		os.Exit(2)
	}
	if r.IsHelp() {
		fmt.Print(r.helpText)
		os.Exit(0)
	}
	return r.rv.Interface().(T)
}

func (p parser[T]) Help() string {
	return p.help
}

func BuildParser[T any](u *T) Parser[T] {
	parse, topHelp := checkTopAndBuildParseFn(u, os.Args[0])
	return parser[T]{parse, topHelp}
}

func checkTopAndBuildParseFn(u any, execName string) (parseFn, string) {
	if err := checkType(u); err != nil {
		panic(err)
	}
	return buildParseFn(
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
		return errors.New("in BuildParser(v), type of v  must be *struct{...}")
	}
	return nil
}

const (
	boolArgLen    = 1
	argLen        = 2
	subcommandLen = 1
)

type kwType int

const (
	boolArg = iota
	valArg
	subcommand
)

type argInfo struct {
	parseFn    parseFn
	fullName   string
	consumeLen int
	ty         kwType
	defaultVal string
	hasDefault bool
	usage      string
}

type cmdInfo struct {
	parseFn    parseFn
	fullName   string
	consumeLen int
	ty         kwType
	usage      string
	fullUsage  string
}

func buildParseFn(
	fieldChain string, viewNameChain string, u reflect.Value,
) (p parseFn, usageText string) {
	argStructPtr := u.Elem() // *T -> T
	structDef := u.Type().Elem()

	arg, command, err := buildArgAndCommandList(
		fieldChain, viewNameChain, structDef, argStructPtr,
	)
	if err != nil {
		panic(err)
	}
	err = validateArgAndCommand(fieldChain, arg, command)
	if err != nil {
		panic(err)
	}

	parse := func(input []string) (parseResult, error) {
		encounter := make(map[string]bool)
		parseRes := parseResult{
			helpText: makeUsageText(viewNameChain, arg, command),
		}
		for len(input) > 0 {
			first, rest := input[0], input[1:]
			switch {
			case strings.HasPrefix(first, "-"):
				realOption := strings.Trim(first, "-/")
				if realOption == "help" {
					parseRes.isHelpCmd = true
					return parseRes, nil
				}
				t, ok := arg[realOption]
				if !ok {
					return parseRes, fmt.Errorf(
						errNoArgument,
						realOption,
						viewNameChain,
					)
				}
				encounter[realOption] = true
				if t.ty == boolArg {
					argStructPtr.FieldByName(t.fullName).Set(
						reflect.ValueOf(
							!strings.HasPrefix(strings.Trim(first, "-"), "/"),
						),
					)
				} else {
					if len(input) < t.consumeLen {
						return parseRes, fmt.Errorf(
							errNoValue, realOption, viewNameChain,
						)
					}
					val := input[1:t.consumeLen]
					r, err := t.parseFn(val)
					if err != nil {
						return parseResult{}, fmt.Errorf(
							errParseArg, realOption, strings.Join(val, ""), err,
						)
					}
					argStructPtr.FieldByName(t.fullName).Set(r.rv)
				}
				input = input[t.consumeLen:]
			default: // subcommand
				t, ok := command[first]
				if !ok {
					return parseRes, fmt.Errorf(errNoSubcommand, first, viewNameChain)
				}
				r, err := t.parseFn(rest)
				if err != nil {
					return r, err
				}
				if r.IsHelp() {
					return r, nil
				}
				argStructPtr.FieldByName(t.fullName).Set(r.rv.Addr())
				input = input[:0] // break for
			}
		}

		for argName, info := range arg {
			if _, argFound := encounter[argName]; !argFound {
				if info.hasDefault {
					r, err := info.parseFn([]string{info.defaultVal})
					if err != nil {
						panic(
							"default values are validated before. " +
								"Cannot parse error at parse",
						)
					}
					argStructPtr.FieldByName(info.fullName).Set(r.rv)
				} else {
					return parseRes, fmt.Errorf(
						errArgNotFound,
						argName,
						viewNameChain,
					)
				}
			}
		}
		parseRes.rv = argStructPtr
		return parseRes, nil
	}
	return parse, makeUsageText(viewNameChain, arg, command)
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

func makeUsageText(viewNameChain string, arg map[string]argInfo, command map[string]cmdInfo) string {
	shiftFour := func(s string) string {
		const fourSpace = "    "
		return fourSpace + s
	}
	fmap := func(ss []string, f func(string) string) []string {
		for i, s := range ss {
			ss[i] = f(s)
		}
		return ss
	}
	appendSpacesToLength := func(s string, toLength int) string {
		needSpace := toLength - len(s)
		for i := 0; i < needSpace; i++ {
			s += " "
		}
		return s
	}

	const helpArg = "-help, --help"
	const boolArgFmt = "--%s, --/%s"
	const generalArgFmt = "--%s <%s>"
	argList := []string{}
	{
		maxArgLength := len(helpArg)
		for _, a := range sortMap(arg) {
			argName := a.key
			argInfo := a.val
			unifiedUsage := argName
			if argInfo.usage != "" {
				unifiedUsage = a.val.usage
			}
			maxArgLength = maxInt(
				maxArgLength,
				func() int {
					if a.val.ty == boolArg {
						return len(fmt.Sprintf(boolArgFmt, argName, argName))
					}
					return len(fmt.Sprintf(generalArgFmt, argName, unifiedUsage))
				}(),
			)
		}
		argList = append(
			argList,
			fmt.Sprintf(
				"%s  %s",
				appendSpacesToLength(helpArg, maxArgLength), "print this message",
			),
		)
		for _, a := range sortMap(arg) {
			info := a.val
			argName := a.key
			argUsage := func() string {
				unifiedUsage := argName
				if info.usage != "" {
					unifiedUsage = info.usage
				}
				if info.ty == boolArg {
					return fmt.Sprintf(
						"%s  %s",
						appendSpacesToLength(
							fmt.Sprintf(boolArgFmt, argName, argName),
							maxArgLength,
						),
						fmt.Sprintf("set [%s] to true / false",
							unifiedUsage,
						),
					)
				}
				return fmt.Sprintf(generalArgFmt, argName, unifiedUsage)
			}()

			if info.hasDefault {
				defaultText := func() string {
					if info.ty == boolArg {
						return fmt.Sprintf(`[default: %s]`, info.defaultVal)
					}
					return fmt.Sprintf(`[default: "%s"]`, info.defaultVal)
				}()
				argUsage = fmt.Sprintf(
					"%s  %s",
					appendSpacesToLength(argUsage, maxArgLength),
					defaultText,
				)
			}
			argList = append(argList, argUsage)
		}
	}

	cmdList := []string{}
	{
		maxCmdLength := 0
		for _, c := range sortMap(command) {
			maxCmdLength = maxInt(maxCmdLength, len(c.key))
		}
		for _, c := range sortMap(command) {
			cmd := c.key
			info := c.val
			cmdUsage := cmd
			if info.usage != "" {
				cmdUsage = fmt.Sprintf(
					"%s  %s",
					appendSpacesToLength(cmd, maxCmdLength),
					info.usage,
				)
			}
			cmdList = append(cmdList, cmdUsage)
		}
	}

	usage := fmt.Sprintf("Usage: %s [OPTIONS]\n", viewNameChain)
	if len(cmdList) > 0 {
		usage = fmt.Sprintf("Usage: %s [OPTIONS] [COMMAND]\n", viewNameChain)
		usage += fmt.Sprintf(
			"\nCommands:\n%s\n", strings.Join(fmap(cmdList, shiftFour), "\n"),
		)
	}
	// arg always have --help, thus not empty
	usage += fmt.Sprintf("\nOptions:\n%s\n", strings.Join(fmap(argList, shiftFour), "\n"))
	if len(cmdList) > 0 {
		usage += fmt.Sprintf(
			"\nRun `%s [COMMAND] -help` to print the help message of COMMAND\n\n",
			viewNameChain,
		)
	}
	return usage
}

func buildArgAndCommandList(
	fieldChain string,
	viewNameChain string,
	structDef reflect.Type, argStructPtr reflect.Value,
) (arg map[string]argInfo, command map[string]cmdInfo, err error) {
	arg = make(map[string]argInfo)     // arguments list
	command = make(map[string]cmdInfo) // commands list

	for i := 0; i < argStructPtr.NumField(); i++ {
		defField := structDef.Field(i)
		defName := defField.Name
		flagName := defField.Tag.Get("flag")
		if flagName == "" {
			flagName = defName
		}
		defaultVal, hasDefault := defField.Tag.Lookup("default")
		usage := defField.Tag.Get("usage")

		valField := argStructPtr.Field(i)
		p, e := hasCustomParser(valField)
		if e == nil { // has implemented Parse
			arg[flagName] = argInfo{
				p,
				defName,
				argLen,
				valArg,
				defaultVal,
				hasDefault,
				usage,
			}
		} else {
			switch valField.Kind() {
			case reflect.Slice:
				ptrToElem := reflect.New(valField.Type().Elem())
				parseElemFn, err := func() (func(reflect.Value) parseFn, error) {
					if _, ok := ptrToElem.Interface().(Parse); ok {
						return customParserFor, nil
					} else {
						switch ptrToElem.Elem().Kind() {
						case reflect.String:
							return func(_ reflect.Value) parseFn {
								return parseString
							}, nil
						case reflect.Bool:
							return func(_ reflect.Value) parseFn {
								return parseBool
							}, nil
						case reflect.Int:
							return func(_ reflect.Value) parseFn {
								return parseInt
							}, nil
						case reflect.Float64:
							return func(_ reflect.Value) parseFn {
								return parseFloat64
							}, nil
						default:
							return nil, fmt.Errorf(
								errNotImplParse,
								ptrToElem.Type(),
								fmt.Sprintf("%s.%s", fieldChain, defName),
							)
						}
					}
				}()
				if err != nil {
					return arg, command, fmt.Errorf(
						errNotImplParse,
						ptrToElem.Type().Elem(),
						fmt.Sprintf("%s.%s", fieldChain, defName),
					)
				}
				arg[flagName] = argInfo{
					func(s []string) (parseResult, error) {
						str := strings.Join(s, "")
						if str == "" {
							return parseResult{
								rv: reflect.MakeSlice(valField.Type(), 0, 0),
							}, nil
						}
						strSlice := strings.Split(str, ",")
						resultSlice := reflect.MakeSlice(
							valField.Type(), len(strSlice), len(strSlice),
						)
						for i, si := range strSlice {
							newElem := reflect.New(ptrToElem.Type().Elem()).Elem()
							res, err := parseElemFn(newElem)([]string{si})
							if err != nil {
								return parseResult{}, err
							}
							resultSlice.Index(i).Set(res.rv)
						}
						return parseResult{rv: resultSlice}, nil
					},
					defName,
					argLen,
					valArg,
					defaultVal,
					hasDefault,
					usage,
				}
			case reflect.String:
				arg[flagName] = argInfo{
					parseString,
					defName,
					argLen,
					valArg,
					defaultVal,
					hasDefault,
					usage,
				}
			case reflect.Bool:
				arg[flagName] = argInfo{
					parseBool,
					defName,
					boolArgLen,
					boolArg,
					defaultVal,
					hasDefault,
					usage,
				}
			case reflect.Int:
				arg[flagName] = argInfo{
					parseInt,
					defName,
					argLen,
					valArg,
					defaultVal,
					hasDefault,
					usage,
				}
			case reflect.Float64:
				arg[flagName] = argInfo{
					parseFloat64,
					defName,
					argLen,
					valArg,
					defaultVal,
					hasDefault,
					usage,
				}
			case reflect.Pointer:
				// should be *struct{...}
				fieldType := valField.Type().Elem() // reflect.Type struct{...}
				if fieldType.Kind() != reflect.Struct {
					return arg, command, fmt.Errorf(
						errNotStructPtr,
						valField.Type(),
						fmt.Sprintf("%s.%s", fieldChain, defName),
					)
				}

				instance := reflect.New(fieldType) // reflect.Value *struct{...}
				parseFn, usageText := buildParseFn(
					fmt.Sprintf("%s.%s", fieldChain, defName),
					fmt.Sprintf("%s %s", viewNameChain, flagName),
					instance)
				command[flagName] = cmdInfo{
					parseFn,
					defName,
					subcommandLen,
					subcommand,
					usage,
					usageText,
				}
			default:
				return arg, command, fmt.Errorf(
					errNotImplParse,
					valField.Type(),
					fmt.Sprintf("%s.%s", fieldChain, defName),
				)
			}
		}
	}
	return arg, command, nil
}

// customParser return parseFn for this specific r: reflect.Value
func hasCustomParser(r reflect.Value) (
	customParser parseFn, err error,
) {
	if !r.CanAddr() {
		return customParser,
			fmt.Errorf("can not addr type %v", r.Type())
	}
	if _, ok := r.Addr().Interface().(Parse); ok {
		return customParserFor(r), nil
	}
	return customParser,
		errors.New("does not implement Parse")
}

// calling this function returns the specific function parse
// and set the reflect.Value r by the Parse method defined for
// type r. Caller MUST ensure r is *Parse.
func customParserFor(r reflect.Value) parseFn {
	return func(s []string) (parseResult, error) {
		e := r.Addr().
			Interface().(Parse).
			FromString(strings.Join(s, ""))
		return parseResult{
			rv: r, // r will be updated by FromString
		}, e
	}
}

func validateArgAndCommand(
	fieldChain string, arg map[string]argInfo, command map[string]cmdInfo,
) error {
	for flagName, info := range arg {
		if flagName == "help" {
			return fmt.Errorf(
				errHelpIsReserved,
				fmt.Sprintf("%s.%s", fieldChain, info.fullName),
			)
		}
		if info.defaultVal != "" {
			_, err := arg[flagName].parseFn([]string{info.defaultVal})
			if err != nil {
				return fmt.Errorf(
					errParseDefault,
					info.defaultVal,
					fmt.Sprintf("%s.%s", fieldChain, info.fullName),
					err,
				)
			}
		}
	}
	return nil
}

func parseBool(s []string) (parseResult, error) {
	b, e := strconv.ParseBool(strings.Join(s, ""))
	return parseResult{
		rv: reflect.ValueOf(b),
	}, e
}

func parseString(s []string) (parseResult, error) {
	return parseResult{
		rv: reflect.ValueOf(strings.Join(s, "")),
	}, nil
}

func parseInt(s []string) (parseResult, error) {
	b, e := strconv.ParseInt(strings.Join(s, ""), 0, 64)
	return parseResult{
		rv: reflect.ValueOf(int(b)),
	}, e
}

func parseFloat64(s []string) (parseResult, error) {
	b, e := strconv.ParseFloat(strings.Join(s, ""), 64)
	return parseResult{
		rv: reflect.ValueOf(b),
	}, e
}
