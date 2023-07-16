package main

import (
	"errors"
	"fmt"
	"strings"

	"github.com/canoriz/scli"
)

// define arguments struct
// define flag, default and usage in struct field's tags
type options struct {
	Host string `flag:"h" usage:"hostname"`
	Port string `flag:"p" default:"80" usage:"port"`

	Num   int     `flag:"n"`
	Ratio float64 `default:"3.14159"`

	Tcp bool `flag:"t" usage:"use tcp"`
	Udp bool `flag:"u" usage:"use udp"`

	Names []string `flag:"nm" default:"alice,bob" usage:"names"`
	Index []int    `flag:"i" default:"1,2,3"`
}

func main() {
	var op options // init a empty argument struct

	// build CLI parser with checkArgumentValidity checker
	// checkArgumentValidity is optional
	scli.Build(&op, checkArgumentValidity).Parse()
	fmt.Printf("parse command line\n%+v\n", op)

	// you can also parse []string
	scli.Build(&op).ParseArgs(strings.Split(
		"-h host.com -n 70 -i 1,3,5 -t -nm cindy,david", " ",
	))
	fmt.Printf("parse []string\n%+v\n", op)
}

// define a post parse checker, return error if none of tcp or udp is enabled,
// Parse() will exit and print usage, ParseArgs() will return this error
func checkArgumentValidity(v *options) error {
	if v.Tcp == v.Udp {
		return errors.New("exactly one of tcp or udp must be enabled")
	}
	return nil
}
