package scli

import (
	"strings"
	"testing"
)

var (
	usageCase = []struct {
		about        string
		work         func() (usage string)
		expectSubStr string
	}{{
		"no argument",
		func() string {
			var s0 struct {
				A0 bool `flag:"a" default:"0" usage:"ua"`
			}
			return BuildParser(&s0).Help()
		},
		`
Options:
	-help, --help  print this message
	--a, --/a      set [ua] to true / false  [default: false]
`,
	}, {
		"only argument",
		func() string {
			var s0 struct {
				A0 string `arg:"a0"`
			}
			return BuildParser(&s0).Help()
		},
		`
Arguments:
	<a0>

Options:
	-help, --help  print this message
`,
	}, {
		"test arg align",
		func() string {
			var s0 struct {
				A0 string `arg:"a0" usage:"arg 0"`
				A1 string `arg:"a1111" usage:"arg 1"`
				A2 string `arg:"a2" usage:"arg 2"`
			}
			return BuildParser(&s0).Help()
		},
		`
Arguments:
	<a0>     arg 0
	<a1111>  arg 1
	<a2>     arg 2

Options:
	-help, --help  print this message
`,
	}, {
		"test option align",
		func() string {
			var s0 struct {
				A0 string `flag:"a0" usage:"arg 0"`
				A1 string `flag:"a1111" usage:"arg 111" default:"11"`
				A2 string `flag:"a2" usage:"arg 2"`
			}
			return BuildParser(&s0).Help()
		},
		`
Options:
	-help, --help      print this message
	--a0 <arg 0>
	--a1111 <arg 111>  [default: "11"]
	--a2 <arg 2>
`,
	}, {
		"test subcmd",
		func() string {
			var s0 struct {
				C1 *struct{} `flag:"c1" usage:"cmd1"`
				C2 *struct{} `flag:"c2" usage:"cmd2"`
			}
			return BuildParser(&s0).Help()
		},
		`
Commands:
	c1  cmd1
	c2  cmd2
`,
	}}
)

func TestUsage(t *testing.T) {
	for _, c := range usageCase {
		t.Run(c.about, func(t *testing.T) {
			helpText := c.work()
			realTrimmed, expTrimmed := trimEveryLine(helpText), trimEveryLine(c.expectSubStr)
			if !strings.Contains(realTrimmed, expTrimmed) {
				t.Fatalf(
					"error: does not contain expected substr\n>>>real>>>\n%s\n===\n%s\n<<<expect<<<\n"+
						">>>real.trimmed>>>\n%s\n===\n%s\n<<<expect.trimmed<<<\n",
					helpText, c.expectSubStr,
					realTrimmed, expTrimmed,
				)
			}
		})
	}
}

func trimEveryLine(s string) string {
	ret := []string{}
	lines := strings.Split(s, "\n")
	for _, l := range lines {
		ret = append(ret, strings.TrimSpace(l))
	}
	return strings.Join(ret, "\n")
}
