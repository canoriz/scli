package scli

import (
	"errors"
	"reflect"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

type panicCase struct {
	about     string
	panicCode func()
	expErr    []string
}

type noExample struct {
	ip string
}

func (n *noExample) FromString(s string) error {
	n.ip = s
	return nil
}

type invalidExample struct {
	ip string
}

func (w *invalidExample) FromString(s string) error {
	if s == "error" {
		return errors.New("can not be error")
	}
	w.ip = s
	return nil
}

func (w *invalidExample) Example() string {
	return "error"
}

type parseOnceTyImpl struct {
	ip string
}

func (w *parseOnceTyImpl) FromString(s string) error {
	if s == "error" {
		return errors.New("can not be error")
	}
	w.ip = s
	return nil
}

func (w *parseOnceTyImpl) Example() string {
	return "error"
}

func (w *parseOnceTyImpl) statefulOrImpure() {}

var (
	// type checker
	_ Parse     = &invalidExample{}
	_ ParseOnce = &parseOnceTyImpl{}
	_ ParseOnce = &MarkOnce[*addr]{}
)

type stringAlias string

func (sa *stringAlias) FromString(s string) error {
	*sa = stringAlias(s)
	return nil
}

type addr struct {
	ip   string
	port string
}

func (a *addr) FromString(s string) error {
	ss := strings.Split(s, ":")
	if len(ss) < 2 {
		return errors.New("x")
	}
	a.ip = ss[0]
	a.port = ss[1]
	return nil
}

func (a *addr) Example() string {
	return "127.0.0.1:8000"
}

type upper string

func (u *upper) FromString(s string) error {
	*u = upper(strings.ToUpper(s))
	return nil
}

func (u *upper) Example() string {
	return "lower"
}

var (
	panicCases = []panicCase{{
		"wrong default",
		func() {
			var s0 struct {
				Float1 float64 `default:"-19.x923"`
			}
			BuildParser(&s0)
		},
		[]string{"error parsing default value"},
	}, {
		"wrong default",
		func() {
			var s0 struct {
				Float1 float64 `flag:"f" default:"-19.x923"`
			}
			BuildParser(&s0)
		},
		[]string{"error parsing default value"},
	}, {
		"wrong default",
		func() {
			var s0 struct {
				I []invalidExample `flag:"f"`
			}
			BuildParser(&s0)
		},
		[]string{"Example()", "cannot parsed by FromString(..)"},
	}, {
		"unexported field",
		func() {
			var s0 struct {
				float1 float64 `default:"-19.923"`
			}
			BuildParser(&s0)
		},
		[]string{"unexported field"},
	}, {
		"nil pointer",
		func() {
			s0 := new(struct {
				Float1 float64 `default:"-19.923"`
			})
			s0 = nil
			BuildParser(s0)
		},
		[]string{"must be non nil"},
	}, {
		"not struct",
		func() {
			var s0 int
			BuildParser(&s0)
		},
		[]string{"must be *struct"},
	}, {
		"custom field not impl Parse",
		func() {
			type a []int8
			var s0 struct {
				V a
			}
			BuildParser(&s0)
		},
		[]string{"did not implement `Parse` interface"},
	}, {
		"not struct pointer",
		func() {
			type a []string
			var s0 struct {
				V a
				U *int
			}
			BuildParser(&s0)
		},
		[]string{"must be *struct{...} to represent a subcommand, or type `Parse`"},
	}, {
		"custom field wrong default",
		func() {
			var s0 struct {
				D addr `default:"133"`
			}
			BuildParser(&s0)
		},
		[]string{"error parsing default val"},
	}, {
		"custom field no example",
		func() {
			var s0 struct {
				D noExample `default:"133"`
			}
			BuildParser(&s0)
		},
		[]string{"did not implement `Parse` interface"},
	}, {
		"custom field invalid example",
		func() {
			var s0 struct {
				D invalidExample `default:"133"`
			}
			BuildParser(&s0)
		},
		[]string{"Example()", "cannot parsed by FromString(..)"},
	}, {
		"help is argument",
		func() {
			var s0 struct {
				Float1 float64 `flag:"help"`
			}
			BuildParser(&s0)
		},
		[]string{`"help" is a reserved word`},
	}, {
		"type alias",
		func() {
			var s0 struct {
				D stringAlias `default:"133"`
			}
			BuildParser(&s0)
		},
		[]string{""},
	}, {
		"type alias in slice",
		func() {
			var s0 struct {
				D []stringAlias `default:"133"`
			}
			BuildParser(&s0)
		},
		[]string{"did not implement `Parse` interface"},
	}, {
		"slice []T where T did not implement Parse",
		func() {
			var s0 struct {
				D []int8
			}
			BuildParser(&s0)
		},
		[]string{"did not implement `Parse` interface"},
	}, {
		"duplicate option",
		func() {
			var s0 struct {
				V0 int `flag:"v"`
				V1 int `flag:"v"`
			}
			BuildParser(&s0)
		},
		[]string{"option v is redefined"},
	}, {
		"duplicate argument",
		func() {
			var s0 struct {
				V0 int `arg:"v"`
				V1 int `arg:"v"`
			}
			BuildParser(&s0)
		},
		[]string{"argument v is redefined"},
	}, {
		"duplicate subcommand",
		func() {
			var s0 struct {
				V0 *struct{} `flag:"v"`
				V1 *struct{} `flag:"v"`
			}
			BuildParser(&s0)
		},
		[]string{"subcommand v is redefined"},
	}, {
		"both argument and subcommand",
		func() {
			var s0 struct {
				V0 int       `arg:"v"`
				V1 *struct{} `flag:"u"`
			}
			BuildParser(&s0)
		},
		[]string{"both defined"}, // TODO: more specific error
	}, {
		"both argument and subcommand",
		func() {
			var s0 struct {
				V1 *struct{} `flag:"u"`
				V0 int       `arg:"v"`
			}
			BuildParser(&s0)
		},
		[]string{"both defined"}, // TODO: more specific error
	}, {
		"required argument after default argument",
		func() {
			var s0 struct {
				V0 int `arg:"v0"`
				V1 int `arg:"v1" default:"1"`
				V2 int `arg:"v2"`
			}
			BuildParser(&s0)
		},
		[]string{"positional argument", "requires a default value"},
	}, {
		"arg default can not parse",
		func() {
			var s0 struct {
				V0 int `arg:"v0"`
				V1 int `arg:"v1" default:"1.33"`
			}
			BuildParser(&s0)
		},
		[]string{"error parsing default value"},
	}, {
		"multiple slice arg",
		func() {
			var s0 struct {
				V0 []string
				V1 []string
			}
			BuildParser(&s0)
		},
		[]string{"more than one slice type argument"},
	}, {
		"arg after slice",
		func() {
			var s0 struct {
				V0 []string
				V1 string
			}
			BuildParser(&s0)
		},
		[]string{"cannot define argument after slice argument"},
	}, {
		"optional arg slice",
		func() {
			var s0 struct {
				V0 []string `arg:"v0" default:"a b"`
			}
			BuildParser(&s0)
		},
		[]string{"slice argument cannot have default value"},
	}, {
		"optional arg slice",
		func() {
			var s0 struct {
				A0 int `default:"3"`
				V0 []string
			}
			BuildParser(&s0)
		},
		[]string{"cannot define slice argument after optional"},
	}}

	parseErrorCase = []struct {
		about        string
		parseErrorFn func(string) error
		input        string
		expectErr    []string // every string in expectErr should be a substring of parseErrorFn(input)
	}{{
		"omit required option",
		func(input string) error {
			var s0 struct {
				Required int `flag:"r1"`             // option is required because it has no default
				Nreq     int `flag:"r2" default:"3"` // can omit
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"-r2 4",
		[]string{`option "--r1" is required`, "but not provided"},
	}, {
		"option no value",
		func(input string) error {
			var s0 struct {
				Op int `flag:"o" default:"3"` // can omit
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"-o",
		[]string{"no value provided for option"},
	}, {
		"option multiple occurance",
		func(input string) error {
			var s0 struct {
				Op int `flag:"o" default:"3"`
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"-o 3 -o 4",
		[]string{"option", "more than once"},
	}, {
		"not defined option",
		func(input string) error {
			var s0 struct {
				Op int `flag:"o" default:"3"` // can omit
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"-f",
		[]string{`option "--f" provided`, "but not defined"},
	}, {
		"not defined cmd",
		func(input string) error {
			var s0 struct {
				Op   int `flag:"o" default:"3"` // can omit
				Cmd1 *struct {
					N1 int `flag:"n1"`
				}
				Cmd2 *struct {
					N2 int `flag:"n2"`
				}
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"f",
		[]string{`subcommand "f" provided`, "but not defined"},
	}, {
		"custom parse error",
		func(input string) error {
			var s0 struct {
				S addr `flag:"s"`
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"-s xx",
		[]string{"error parsing argument of option"},
	}, {
		"option parse error",
		func(input string) error {
			var s0 struct {
				S int `flag:"s"`
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"-s xx",
		[]string{"error parsing argument of option"},
	}, {
		"arg parse error",
		func(input string) error {
			var s0 struct {
				V string `arg:"f"`
				S int    `arg:"s"`
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"xx xx",
		[]string{"error parsing positional argument"},
	}, {
		"slice option parse error",
		func(input string) error {
			var s0 struct {
				S []addr `flag:"s"`
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"-s 14$3",
		[]string{"error parsing argument of option"},
	}, {
		"arg option parse error",
		func(input string) error {
			var s0 struct {
				S []int
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"11 14$3",
		[]string{"error parsing positional argument"},
	}, {
		"missing arg",
		func(input string) error {
			var s0 struct {
				Arg0 string `arg:"a0"`
				Arg1 string `arg:"a1"`
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"only-a0",
		[]string{"argument <a1> is required", "but not provided"},
	}, {
		"too many arg",
		func(input string) error {
			var s0 struct {
				Arg0 string `arg:"a0"`
				Arg1 string `arg:"a1"`
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"a0 a1 a2",
		[]string{"too many args"},
	}, {
		"extra input",
		func(input string) error {
			var s0 struct {
				Op   int `flag:"o" default:"3"` // can omit
				Cmd1 *struct {
					N1 int `flag:"n1"`
				} `flag:"cmd1"`
				Cmd2 *struct {
					N2 int `flag:"n2"`
				} `flag:"cmd2"`
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"-o 4 cmd1 -n1 4 cmd3",
		[]string{"maybe remove"},
	}, {
		"undefined option",
		func(input string) error {
			var s0 struct {
				Op   int `flag:"o" default:"3"` // can omit
				Cmd1 *struct {
					N1 int `flag:"n1"`
				} `flag:"cmd1"`
				Cmd2 *struct {
					N2 int `flag:"n2"`
				} `flag:"cmd2"`
			}
			_, err := BuildParser(&s0).ParseArg(input)
			return err
		},
		"-o 4 cmd1 -n1 4 -af",
		[]string{`option "--af" provided in`, "not defined"},
	}}
	// TODO: add file tests

	parseOkCase = []struct {
		about string
		work  func() (_real any, _exp any, _ error)
	}{{
		"parse arg starts with dash",
		func() (any, any, error) {
			type ty struct {
				A0 string `arg:"a0"`
				A1 string `arg:"a1"`
			}
			var s0 ty
			s1 := ty{
				A0: "-a00",
				A1: "-a11",
			}
			_, err := BuildParser(&s0).ParseArg("-a00 -a11")
			return s0, s1, err
		},
	}, {
		"parse args and options",
		func() (any, any, error) {
			type ty struct {
				A0 string `arg:"a0"`
				F1 string `flag:"f1"`
			}
			var s0 ty
			s1 := ty{
				A0: "a00",
				F1: "f11",
			}
			_, err := BuildParser(&s0).ParseArg("-f1 f11 a00")
			return s0, s1, err
		},
	}, {
		"parse args and options, args starts with dash",
		func() (any, any, error) {
			type ty struct {
				A0 string `arg:"a0"`
				A1 string `arg:"a1"`
				F1 string `flag:"f1" default:"def"`
			}
			var s0 ty
			s1 := ty{
				A0: "a00",
				A1: "-f1",
				F1: "def",
			}
			_, err := BuildParser(&s0).ParseArg("a00 -f1")
			return s0, s1, err
		},
	}, {
		"parse many types",
		func() (any, any, error) {
			type ty struct {
				S0 string  `arg:"s0"`
				I1 int     `arg:"i1"`
				B2 bool    `arg:"b2"`
				F3 float64 `arg:"f3"`

				C0  addr     `arg:"c0"`
				SS0 []string `arg:"ss0"`
			}
			var s0 ty
			s1 := ty{
				S0: "a00",
				I1: 42,
				B2: false,
				F3: -1.2345,

				C0: addr{
					ip:   "127.0.0.1",
					port: "3000",
				},
				SS0: []string{"a3", "a4"},
			}
			_, err := BuildParser(&s0).ParseArg(
				"a00 42 false -1.2345 127.0.0.1:3000 a3 a4",
			)
			return s0, s1, err
		},
	}, {
		"arg value same as option",
		func() (any, any, error) {
			type ty struct {
				S0  string `arg:"s0"`
				S1  string `arg:"s1"`
				Op2 int    `flag:"op2" default:"42"`
			}
			var s0 ty
			s1 := ty{
				S0:  "a00",
				S1:  "-op2",
				Op2: 42,
			}
			_, err := BuildParser(&s0).ParseArg("a00 -op2")
			return s0, s1, err
		},
	}, {
		"arg value same as option",
		func() (any, any, error) {
			type ty struct {
				S0  string `arg:"s0"`
				S1  string `arg:"s1"`
				Op2 int    `flag:"op2" default:"42"`
			}
			var s0 ty
			s1 := ty{
				S0:  "-op2",
				S1:  "-op2",
				Op2: 42,
			}
			_, err := BuildParser(&s0).ParseArg("-op2 -op2")
			return s0, s1, err
		},
	}, {
		"default value arg",
		func() (any, any, error) {
			type ty struct {
				S0  string `arg:"s0"`
				S1  string `arg:"s1"`
				S2  string `arg:"s2" default:"s2"`
				S3  string `arg:"s3" default:"s3"`
				Op2 int    `flag:"op2" default:"42"`
			}
			var s0 ty
			s1 := ty{
				S0:  "s00",
				S1:  "s11",
				S2:  "s22",
				S3:  "s3",
				Op2: 42,
			}
			_, err := BuildParser(&s0).ParseArg("s00 s11 s22")
			return s0, s1, err
		},
	}, {
		"parse arg []string",
		func() (any, any, error) {
			type ty struct {
				A0 string
				A1 []string
			}
			var s0 ty
			s1 := ty{
				A0: "a00",
				A1: []string{"a00", "-a11"},
			}
			_, err := BuildParser(&s0).ParseArg("a00 a00 -a11")
			return s0, s1, err
		},
	}, {
		"parse arg []int",
		func() (any, any, error) {
			type ty struct {
				A1 []int
			}
			var s0 ty
			s1 := ty{
				A1: []int{0, 1, 2},
			}
			_, err := BuildParser(&s0).ParseArg("0 1 2")
			return s0, s1, err
		},
	}, {
		"parse arg []bool",
		func() (any, any, error) {
			type ty struct {
				A1 []bool
			}
			var s0 ty
			s1 := ty{
				A1: []bool{false, false, true, true},
			}
			_, err := BuildParser(&s0).ParseArg("false F T 1")
			return s0, s1, err
		},
	}, {
		"parse arg []float64",
		func() (any, any, error) {
			type ty struct {
				A1 []float64
			}
			var s0 ty
			s1 := ty{
				A1: []float64{-1.5, -1.3, 1.5, 1.1},
			}
			_, err := BuildParser(&s0).ParseArg("-1.5 -1.3 1.5 1.1")
			return s0, s1, err
		},
	}, {
		"parse arg []addr",
		func() (any, any, error) {
			type ty struct {
				A1 []addr
			}
			var s0 ty
			s1 := ty{
				A1: []addr{{
					ip:   "127.0.0.1",
					port: "1000",
				}, {
					ip:   "127.0.0.2",
					port: "1001",
				}},
			}
			_, err := BuildParser(&s0).ParseArg(" 127.0.0.1:1000  127.0.0.2:1001 ")
			return s0, s1, err
		},
	}, {
		"parse empty arg array",
		func() (any, any, error) {
			type ty struct {
				A0 string
				A1 []string
			}
			var s0 ty
			s1 := ty{
				A0: "a00",
				A1: nil,
			}
			_, err := BuildParser(&s0).ParseArg("a00")
			return s0, s1, err
		},
	}, {
		"parse empty arg array",
		func() (any, any, error) {
			type ty struct {
				A0 string
				A1 []addr
			}
			var s0 ty
			s1 := ty{
				A0: "a00",
				A1: nil,
			}
			_, err := BuildParser(&s0).ParseArg("a00")
			return s0, s1, err
		},
	}, {
		"parse once example",
		func() (any, any, error) {
			type ty struct {
				A0 parseOnceTyImpl
			}
			var s0 ty
			s1 := ty{
				A0: parseOnceTyImpl{"a00"},
			}
			_, err := BuildParser(&s0).ParseArg("a00")
			return s0, s1, err
		},
	}}
	// TODO: add file tests
)

func TestPanic(t *testing.T) {
	for _, c := range panicCases {
		t.Run(c.about, func(t *testing.T) {
			defer func() {
				r := recover()
				if r == nil {
					t.Fatal("The code did not panic")
				}
				errStr := func() string {
					if s, ok := r.(string); ok {
						return s
					}
					if s, ok := r.(error); ok {
						return s.Error()
					}
					return "failed"
				}()
				for _, expectStr := range c.expErr {
					if !strings.Contains(errStr, expectStr) {
						t.Fatalf(
							"panic \n%s\ndoes not contain expected substr\n%s",
							errStr, expectStr,
						)
					}
				}
				t.Log(r)
			}()
			c.panicCode()
		})
	}
}

func TestParseError(t *testing.T) {
	for _, c := range parseErrorCase {
		t.Run(c.about, func(t *testing.T) {
			err := c.parseErrorFn(c.input)
			if err == nil {
				t.Fatal("parse should error")
			}
			errstr := err.Error()
			for _, expectStr := range c.expectErr {
				if !strings.Contains(errstr, expectStr) {
					t.Fatalf(
						"error \n%s\ndoes not contain expected substr\n%s",
						errstr, expectStr,
					)
				}
			}
			t.Log(err)
		})
	}
}

func TestParseOk2(t *testing.T) {
	for _, c := range parseOkCase {
		t.Run(c.about, func(t *testing.T) {
			out, exp, err := c.work()
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			assert.Equal(t, out, exp)
		})
	}
}

func TestCheckError(t *testing.T) {
	type A struct {
		A string `flag:"a"`
		B string `flag:"b"`
	}
	var a A
	parser := BuildParser(&a).Checker(
		func(a A) error {
			if len(a.A) < len(a.B) {
				return errors.New("len(a) should greater than len(b)")
			}
			return nil
		},
	)
	if _, err := parser.ParseArg("--a 333 --b 4444"); err == nil {
		t.Fatal("expect check fail, but passed")
	}
	if _, err := parser.ParseArg("--a 4444 --b 333"); err != nil {
		t.Fatalf("should ok, but get %v", err)
	}

}

type Arg struct {
	Size   int   `flag:"sz" default:"12" usage:"block size"`
	VSize  int   `flag:"vsz" usage:"vblock size"`
	Source addr  `flag:"s" default:"127.0.0.1:1001" usage:"destination"`
	Big    upper `flag:"big" default:"qwerty" usage:"to upper case"`
	Add    *add  `flag:"add"`
	Delete *struct {
		Name int `flag:"n" usage:"delete file"`
	} // `flag:"delete"`
	Change *struct {
		Name int `arg:"NAME" usage:"change file"`
	} `flag:"change"`
	DefaultEmptyStr string `flag:"str" default:"" usage:"some string"`
	AddrList        []addr `flag:"addr-list" default:"" usage:"address list"`
}

type add struct {
	Name string `flag:"n"  usage:"add file"`
	B    bool   `flag:"b"`
	High *high  `flag:"high"`
}

type high struct {
	Name string `flag:"n" default:"no" usage:"number"`
}

func TestParseOk(t *testing.T) {
	var a Arg
	{
		input := "-sz  14 -big qwerty -vsz 3 -s " +
			"18:13 -addr-list 18:13,14:12,15:11 add -n  a -/b high -n af "
		expected := Arg{
			Size:            14,
			VSize:           3,
			Big:             "QWERTY",
			Source:          addr{ip: "18", port: "13"},
			Add:             &add{Name: "a", B: false, High: &high{Name: "af"}},
			DefaultEmptyStr: "",
			AddrList: []addr{
				{"18", "13"},
				{"14", "12"},
				{"15", "11"},
			},
		}
		parser := BuildParser(&a)
		r, err := parser.ParseArg(input)
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(expected, a) {
			t.Fatalf("expected: %+v, get %+v", expected, r)
		}
		if parser.Help() == "" {
			t.Fatal("no help message, should have help")
		}
	}

	{
		parser := BuildParser(&a)
		_, err := parser.ParseArg("--help")
		if !errors.Is(err, ErrIsHelp) {
			t.Fatalf("should be help, get %v", err)
		}
	}
}
