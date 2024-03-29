package scli

import (
	"errors"
	"reflect"
	"strings"
	"testing"
)

type panicCase struct {
	about     string
	panicCode func()
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
		}}, {
		"nil pointer",
		func() {
			s0 := new(struct {
				Float1 float64 `default:"-19.923"`
			})
			s0 = nil
			BuildParser(s0)
		}}, {
		"not struct",
		func() {
			var s0 int
			BuildParser(&s0)
		}}, {
		"custom field not impl Parse",
		func() {
			type a []int8
			var s0 struct {
				V a
			}
			BuildParser(&s0)
		}}, {
		"not struct pointer",
		func() {
			type a []string
			var s0 struct {
				V a
				U *int
			}
			BuildParser(&s0)
		}}, {
		"custom field wrong default",
		func() {
			var s0 struct {
				D addr `default:"133"`
			}
			BuildParser(&s0)
		}}, {
		"custom field no example",
		func() {
			var s0 struct {
				D noExample `default:"133"`
			}
			BuildParser(&s0)
		}}, {
		"custom field invalid example",
		func() {
			var s0 struct {
				D invalidExample `default:"133"`
			}
			BuildParser(&s0)
		}}, {
		"help is argument",
		func() {
			var s0 struct {
				Float1 float64 `flag:"help"`
			}
			BuildParser(&s0)
		}}, {
		"type alias",
		func() {
			var s0 struct {
				D stringAlias `default:"133"`
			}
			BuildParser(&s0)
		}}, {
		"type alias in slice",
		func() {
			var s0 struct {
				D []stringAlias `default:"133"`
			}
			BuildParser(&s0)
		}}, {
		"slice did not implement Parse",
		func() {
			var s0 struct {
				D []int8
			}
			BuildParser(&s0)
		}},
	}

	parseError = []struct {
		about string
		arg   string
	}{{
		"no required option",
		"add -n 2 -/b high",
	}, {
		"option no value",
		"-vsz",
	}, {
		"not defined option",
		"-fff",
	}, {
		"not defined cmd",
		"fffcmd",
	}, {
		"sub cmd error",
		"-vsz 12 add -n 2 -/b low",
	}, {
		"custom parse error",
		"-vsz 12 -s xx",
	}, {
		"parse error",
		"-vsz a",
	}, {
		"slice parse error",
		"-vsz 12 -addr-list 14$3",
	}}
)

func TestPanic(t *testing.T) {
	for _, c := range panicCases {
		t.Run(c.about, func(t *testing.T) {
			defer func() {
				if r := recover(); r == nil {
					t.Error("The code did not panic")
				}
			}()
			c.panicCode()
		})
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
	}
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

func TestParseError(t *testing.T) {
	var a Arg
	for _, c := range parseError {
		t.Run(c.about, func(t *testing.T) {
			_, err := BuildParser(&a).ParseArg(c.arg)
			if err == nil {
				t.Error("should error")
			}
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
		t.Error("expect check fail, but passed")
	}
	if _, err := parser.ParseArg("--a 4444 --b 333"); err != nil {
		t.Errorf("should ok, but get %v", err)
	}

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
			t.Error(err)
		}
		if !reflect.DeepEqual(expected, a) {
			t.Errorf("expected: %+v, get %+v", expected, r)
		}
		if parser.Help() == "" {
			t.Log(parser.Help())
			t.Error("should have help")
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
