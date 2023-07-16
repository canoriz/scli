package scli

import (
	"errors"
	"flag"
	"fmt"
	"os"
	"reflect"
	"strconv"
	"strings"
)

var (
	errNotSupportedSliceType = "in Build(&struct{ ...; v []Type; ...}), Type must be int|float64|string|bool, found: %v %v"
	errDefaultValue          = "parse default value (%v %v = %v) error"
	errNotSupportedType      = "in Build(&struct{ ...; v Type; ...}), Type must be int|float64|string|bool, found: %v %v"
	errPostCheck             = "Argument parse success, post check failed: %v\n"
)

type postCheckErr struct{ e error }

func (p postCheckErr) Error() string {
	return p.e.Error()
}

type copyArg struct {
	s  any // s: slice of *[]T
	sa any // sa: slice of *arrOf[T]
}

type parser[T any] struct {
	fs              *flag.FlagSet
	copyAfterParsed []copyArg

	structPtr T
	postCheck []func(T) error
}

// Parse() parses flags from command line
func (p parser[T]) Parse() {
	if err := p.ParseArgs(os.Args[1:]); err != nil {
		if err == flag.ErrHelp {
			os.Exit(0)
		}
		if errors.As(err, &postCheckErr{}) {
			fmt.Fprintf(os.Stderr, errPostCheck, err)
			p.Usage()
		}
		os.Exit(2)
	}
}

// ParseArgs([]string) parses flags from input
func (p parser[T]) ParseArgs(args []string) error {
	if err := p.fs.Parse(args); err != nil {
		return err
	}
	for _, v := range p.copyAfterParsed {
		switch v.s.(type) {
		case *[]string:
			*v.s.(*[]string) = v.sa.(*arrOf[string]).arr
		case *[]int:
			*v.s.(*[]int) = v.sa.(*arrOf[int]).arr
		case *[]float64:
			*v.s.(*[]float64) = v.sa.(*arrOf[float64]).arr
		case *[]bool:
			*v.s.(*[]bool) = v.sa.(*arrOf[bool]).arr
		}
	}
	for _, f := range p.postCheck {
		if err := f(p.structPtr); err != nil {
			return postCheckErr{err}
		}
	}
	return nil
}

func (p parser[T]) Usage() {
	p.fs.Usage()
}

func buildSliceParser[T bool | int | string | float64, StructT any](
	p *parser[StructT],
	flagName, defaultVal, usage string,
	valField reflect.Value,
	fieldName string,
) {
	var z arrOf[T]
	hasDefault := defaultVal != ""
	if hasDefault {
		if err := z.Set(defaultVal); err != nil {
			panic(fmt.Sprintf(errDefaultValue, fieldName, valField.Type().String(), defaultVal))
		}
	}
	p.fs.Var(&z, flagName, usage)
	p.copyAfterParsed = append(p.copyAfterParsed, copyArg{
		s:  valField.Addr().Interface().(*[]T),
		sa: &z,
	})
}

func Build[T any](u T, postCheck ...func(T) error) parser[T] {
	ptr := reflect.ValueOf(u) // any -> *T
	if ptr.Kind() != reflect.Pointer || ptr.IsNil() {
		panic("in Build(v), type(v) must be non nil *struct{...}")
	}
	argStructPtr := ptr.Elem() // *T -> T
	if argStructPtr.Kind() != reflect.Struct {
		panic("in Build(v), type(v) must be *struct{...}")
	}
	structDef := argStructPtr.Type()

	ret := parser[T]{
		fs: flag.NewFlagSet(os.Args[0], flag.ContinueOnError),

		structPtr: u,
		postCheck: postCheck,
	}

	for i := 0; i < argStructPtr.NumField(); i++ {
		defField := structDef.Field(i)
		flagName := defField.Tag.Get("flag")
		if flagName == "" {
			flagName = defField.Name
		}
		defaultVal := defField.Tag.Get("default")
		usage := defField.Tag.Get("usage")

		valField := argStructPtr.Field(i)
		switch valField.Kind() {
		case reflect.String:
			z := valField.Addr().Interface().(*string)
			ret.fs.StringVar(z, flagName, defaultVal, usage)
		case reflect.Bool:
			z := valField.Addr().Interface().(*bool)
			ret.fs.BoolVar(
				z, flagName,
				parseDefaultOrZero(
					valField.Kind(),
					valField.Type(),
					defField.Name, defaultVal,
				).(bool),
				usage,
			)
		case reflect.Int:
			z := valField.Addr().Interface().(*int)
			ret.fs.IntVar(
				z, flagName,
				parseDefaultOrZero(
					valField.Kind(),
					valField.Type(),
					defField.Name, defaultVal,
				).(int),
				usage,
			)
		case reflect.Float64:
			z := valField.Addr().Interface().(*float64)
			ret.fs.Float64Var(
				z, flagName,
				parseDefaultOrZero(
					valField.Kind(),
					valField.Type(),
					defField.Name, defaultVal,
				).(float64),
				usage,
			)
		case reflect.Slice:
			usage = fmt.Sprintf(
				"%v %v, input by %v[,%v]...", usage,
				valField.Type().String(),
				valField.Type().Elem().String(), valField.Type().Elem().String(),
			)
			switch valField.Type().Elem().Kind() {
			case reflect.String:
				buildSliceParser[string](
					&ret,
					flagName, defaultVal, usage,
					valField, defField.Name,
				)
			case reflect.Int:
				buildSliceParser[int](
					&ret,
					flagName, defaultVal, usage,
					valField, defField.Name,
				)
			case reflect.Bool:
				buildSliceParser[bool](
					&ret,
					flagName, defaultVal, usage,
					valField, defField.Name,
				)
			case reflect.Float64:
				buildSliceParser[float64](
					&ret,
					flagName, defaultVal, usage,
					valField, defField.Name,
				)
			default:
				panic(fmt.Errorf(
					errNotSupportedSliceType,
					defField.Name,
					valField.Type().String(),
				))
			}
		default:
			panic(fmt.Errorf(
				errNotSupportedType,
				defField.Name,
				valField.Type().String(),
			))
		}
	}
	return ret
}

func parseDefaultOrZero(
	kind reflect.Kind,
	typ reflect.Type,
	atField, defaultVal string,
) any {
	hasDefault := defaultVal != ""
	var v any
	var err error
	switch kind {
	case reflect.String:
		if !hasDefault {
			return ""
		}
		v = defaultVal
	case reflect.Int:
		if !hasDefault {
			return 0
		}
		v, err = func(i int64, e error) (int, error) {
			return int(i), e
		}(strconv.ParseInt(defaultVal, 0, 64))
	case reflect.Float64:
		if !hasDefault {
			return 0
		}
		v, err = strconv.ParseFloat(defaultVal, 64)
	case reflect.Bool:
		if !hasDefault {
			return false
		}
		v, err = strconv.ParseBool(defaultVal)
	default:
		panic("unsupported type")
	}
	if err != nil {
		panic(fmt.Sprintf(errDefaultValue, atField, typ.String(), defaultVal))
	}
	return v
}

type arrOf[T bool | int | string | float64] struct {
	arr []T
}

func (a *arrOf[T]) String() string {
	sslice := make([]string, len(a.arr))
	for i, v := range a.arr {
		sslice[i] = fmt.Sprintf("%v", v)
	}
	return strings.Join(sslice, ",")
}

func (a *arrOf[T]) Set(s string) error {
	var zero T
	a.arr = []T{} // clear default value
	type appendElemFn func(*arrOf[T], string) error
	appendElemFnBuilder := func(parseFn func(string) (any, error)) appendElemFn {
		return func(a *arrOf[T], t string) error {
			n, err := parseFn(t)
			if err != nil {
				return err
			}
			a.arr = append(a.arr, n.(T))
			return nil
		}
	}
	var appendElem appendElemFn

	switch reflect.TypeOf(zero).Kind() {
	case reflect.Int:
		appendElem = appendElemFnBuilder(func(s string) (any, error) {
			i64, e := strconv.ParseInt(s, 0, 64)
			return int(i64), e
		})
	case reflect.Bool:
		appendElem = appendElemFnBuilder(func(s string) (any, error) {
			return strconv.ParseBool(s)
		})
	case reflect.Float64:
		appendElem = appendElemFnBuilder(func(s string) (any, error) {
			return strconv.ParseFloat(s, 64)
		})
	case reflect.String:
		appendElem = appendElemFnBuilder(func(s string) (any, error) {
			return s, nil
		})
	default:
		return fmt.Errorf(
			errNotSupportedSliceType,
			reflect.TypeOf(zero).String(),
		)
	}
	elems := strings.Split(s, ",")
	for _, v := range elems {
		if v != "" {
			if err := appendElem(a, v); err != nil {
				return err
			}
		}
	}
	return nil
}
