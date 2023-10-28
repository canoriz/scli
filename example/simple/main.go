package main

import (
	"fmt"
	"strings"

	"github.com/canoriz/scli"
)

// define arguments struct
// define flag, default and usage in struct field's tags
type options struct {
	Print bool `flag:"p" default:"true" usage:"print result"`

	// subcommands are defined by *struct{...}
	Add *struct {
		N1 int `default:"0"`
		N2 int
	} `flag:"add" usage:"n1+n2"`
	Sub *struct {
		N1 int
		N2 int
	} `flag:"sub" usage:"n1-n2"`
	Mul *struct {
		N1 int
		N2 int
	} `flag:"mul" usage:"n1*n2"`
	Div *struct {
		N1 float64
		N2 float64
	} // leave Div with no tags
}

func main() {
	var op options // init a empty argument struct

	parser := scli.BuildParser(&op)
	// if Parse() error, program exits
	// after this, op == op2
	op = parser.Parse()

	if op.Add != nil {
		printIfTrue(op.Print, op.Add.N1+op.Add.N2)
	} else if op.Sub != nil {
		printIfTrue(op.Print, op.Sub.N1-op.Sub.N2)
	} else if op.Mul != nil {
		printIfTrue(op.Print, op.Mul.N1*op.Mul.N2)
	} else if op.Div != nil {
		printIfTrue(op.Print, op.Div.N1/op.Div.N2)
	} else {
		fmt.Println("exactly one subcommand should be given")
	}

	// parseArg can parse from []string instead of from CLI
	parser.ParseArg(strings.Split("-p add -N1 3 -N2 4", " "))
}

func printIfTrue(p bool, v any) {
	if p {
		fmt.Println(v)
	}
}
