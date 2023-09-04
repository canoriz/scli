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

var (
	ErrIsHelp = errors.New("argument is help message")
)

const ( // build time errors
	errParseDefault   = `error parsing default value "%s" of field "%s", error: %w`
	errHelpIsReserved = `"help" is a reserved word, please change option name of "%s"`
	errNotImplParse   = "type of field %s did not implement `Parse` interface"
)

const ( // runtime errors
	errNoValue      = `no value provided for argument "--%s" in %s`
	errNoArgument   = `argument "--%s" provided in "%s" but not defined`
	errNoSubcommand = `subcommand "%s" provided in "%s" but not defined`
	errParseArg     = `error parsing argument "--%s %s" error: %w`
	errArgNotFound  = `argument "--%s" is required in "%s" but not provided`
)

type Subcommand string

type parseResult struct {
	rv          reflect.Value
	subCmdChain []string
	helpText    string
}

func (p parseResult) IsHelp() bool {
	return p.helpText != ""
}

type parseFn func([]string) (parseResult, error)

type Parser[T any] struct {
	parse parseFn
	help  string
}

func (p Parser[T]) ParseArg(s []string) (zero T, cmdChain []string, err error) {
	r, e := p.parse(s)
	if e != nil {
		return zero, cmdChain, e
	}
	if r.IsHelp() {
		return zero, cmdChain, ErrIsHelp
	}
	return r.rv.Interface().(T), r.subCmdChain, nil
}

func (p Parser[T]) Parse() (t T, cmdChain []string) {
	r, e := p.parse(os.Args[1:])
	if e != nil {
		fmt.Fprintln(os.Stderr, e)
		os.Exit(-2)
	}
	if r.IsHelp() {
		fmt.Print(r.helpText)
		os.Exit(0)
	}
	return r.rv.Interface().(T), r.subCmdChain
}

func (p Parser[T]) Help() string {
	return p.help
}

func BuildParser[T any](u *T) Parser[T] {
	parse, help := checkTopAndBuildParseFn(u, os.Args[0])
	return Parser[T]{parse, help}
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

func buildParseFn(fieldChain string, viewNameChain string, u reflect.Value) (p parseFn, usageText string) {
	argStructPtr := u.Elem() // *T -> T
	structDef := argStructPtr.Type()

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
		var ret parseResult
		for len(input) > 0 {
			first, rest := input[0], input[1:]
			switch {
			case strings.HasPrefix(first, "-"):
				realOption := strings.Trim(first, "-/")
				if realOption == "help" {
					return parseResult{
						helpText: makeUsageText(viewNameChain, arg, command),
					}, nil
				}
				t, ok := arg[realOption]
				if !ok {
					return parseResult{}, fmt.Errorf(
						errNoArgument,
						first,
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
						return parseResult{}, fmt.Errorf(
							errNoValue, realOption, viewNameChain,
						)
					}
					val := input[1:t.consumeLen]
					r, err := t.parseFn(val)
					if err != nil {
						return parseResult{}, fmt.Errorf(
							errParseArg, first, strings.Join(val, ""), err,
						)
					}
					argStructPtr.FieldByName(t.fullName).Set(r.rv)
				}
				input = input[t.consumeLen:]
			default: // subcommand
				t, ok := command[first]
				if !ok {
					return parseResult{},
						fmt.Errorf(errNoSubcommand, first, viewNameChain)
				}
				r, err := t.parseFn(rest)
				if err != nil {
					return parseResult{}, err
				}
				if r.IsHelp() {
					return r, nil
				}
				argStructPtr.FieldByName(t.fullName).Set(r.rv)
				ret.subCmdChain = append(
					[]string{first}, r.subCmdChain...,
				)
				input = input[:0] // break for
			}
		}

		for argName, info := range arg {
			if _, ok := encounter[argName]; !ok {
				if info.defaultVal != "" {
					r, err := info.parseFn([]string{info.defaultVal})
					if err != nil {
						panic(
							"default values are validated before. " +
								"Cannot parse error at parse",
						)
					}
					argStructPtr.FieldByName(info.fullName).Set(r.rv)
				} else {
					return parseResult{}, fmt.Errorf(
						errArgNotFound,
						argName,
						viewNameChain,
					)
				}
			}
		}
		ret.rv = argStructPtr
		return ret, nil
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
	argList := []string{"-help, --help", shiftFour("show this help message")}
	for _, a := range sortMap(arg) {
		info := a.val
		argName := a.key
		argUsage := func() string {
			if info.ty == boolArg {
				if info.usage != "" {
					return fmt.Sprintf(
						"--%s ([%s] true) --/%s ([%s] false)",
						argName, info.usage,
						argName, info.usage,
					)
				}
				return fmt.Sprintf(
					"--%s (true) --/%s (false)",
					argName, argName,
				)
			}
			if info.usage != "" {
				return fmt.Sprintf(
					"--%s <%s>",
					argName, info.usage,
				)
			}
			return fmt.Sprintf(
				"--%s <%s>",
				argName, argName,
			)
		}()
		argList = append(argList, argUsage)
		if info.defaultVal != "" {
			argList = append(
				argList,
				shiftFour(fmt.Sprintf("[default: %s]", info.defaultVal)),
			)
		}
	}
	cmdList := []string{}
	for _, c := range sortMap(command) {
		cmd := c.key
		info := c.val
		cmdList = append(cmdList, cmd)
		if info.usage != "" {
			cmdList = append(cmdList, shiftFour(info.usage))
		}
	}
	usage := fmt.Sprintf("Usage: %s [OPTIONS]\n\n", viewNameChain)
	if len(cmdList) > 0 {
		usage = fmt.Sprintf("Usage: %s [OPTIONS] [COMMAND]\n\n", viewNameChain)
		usage += fmt.Sprintf(
			"Commands:\n%s\n", strings.Join(fmap(cmdList, shiftFour), "\n"),
		)
		usage += fmt.Sprintf("Run `%s [COMMAND] -help` to see command help message\n\n", viewNameChain)
	}
	// arg always have --help, not empty
	return usage + fmt.Sprintf("Options:\n%s\n", strings.Join(fmap(argList, shiftFour), "\n"))
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
		defaultVal := defField.Tag.Get("default")
		usage := defField.Tag.Get("usage")

		valField := argStructPtr.Field(i)
		switch valField.Kind() {
		case reflect.String:
			arg[flagName] = argInfo{
				func(s []string) (parseResult, error) {
					return parseResult{
						rv: reflect.ValueOf(strings.Join(s, "")),
					}, nil
				},
				defName,
				argLen,
				valArg,
				defaultVal,
				usage,
			}
		case reflect.Bool:
			arg[flagName] = argInfo{
				func(s []string) (parseResult, error) {
					b, e := strconv.ParseBool(strings.Join(s, ""))
					return parseResult{
						rv: reflect.ValueOf(b),
					}, e
				},
				defName,
				boolArgLen,
				boolArg,
				defaultVal,
				usage,
			}
		case reflect.Int:
			arg[flagName] = argInfo{
				func(s []string) (parseResult, error) {
					b, e := strconv.ParseInt(strings.Join(s, ""), 0, 64)
					return parseResult{
						rv: reflect.ValueOf(int(b)),
					}, e
				},
				defName,
				argLen,
				valArg,
				defaultVal,
				usage,
			}
		case reflect.Float64:
			arg[flagName] = argInfo{
				func(s []string) (parseResult, error) {
					b, e := strconv.ParseFloat(strings.Join(s, ""), 64)
					return parseResult{
						rv: reflect.ValueOf(b),
					}, e
				},
				defName,
				argLen,
				valArg,
				defaultVal,
				usage,
			}
		case reflect.Struct:
			p, e := customParser(valField)
			if e == nil {
				arg[flagName] = argInfo{
					p,
					defName,
					argLen,
					valArg,
					defaultVal,
					usage,
				}
			} else {
				parseFn, usageText := buildParseFn(
					fmt.Sprintf("%s.%s", fieldChain, defName),
					fmt.Sprintf("%s %s", viewNameChain, flagName),
					valField.Addr())
				command[flagName] = cmdInfo{
					parseFn,
					defName,
					subcommandLen,
					subcommand,
					usage,
					usageText,
				}
			}
		default:
			p, e := customParser(valField)
			if e != nil {
				return arg, command, fmt.Errorf(
					errNotImplParse,
					fmt.Sprintf("%s.%s", fieldChain, defName),
				)
			}
			arg[flagName] = argInfo{
				p,
				defName,
				argLen,
				valArg,
				defaultVal,
				usage,
			}
		}
	}
	return arg, command, nil
}

func customParser(r reflect.Value) (p parseFn, err error) {
	if implParse, ok := r.Addr().Interface().(Parse); ok {
		return func(s []string) (parseResult, error) {
			e := implParse.FromString(strings.Join(s, ""))
			return parseResult{
				rv: r, // r will be updated by FromString
			}, e
		}, nil
	}
	return p, errors.New("x")
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
