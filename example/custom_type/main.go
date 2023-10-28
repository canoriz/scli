package main

import (
	"errors"
	"fmt"
	"strings"

	"github.com/canoriz/scli"
)

type arr []string

func (a *arr) FromString(s string) error {
	*a = strings.Split(s, ",")
	return nil
}

type addr struct {
	ip   string
	port string
}

func (a *addr) FromString(s string) error {
	r := strings.Split(s, ":")
	if len(r) < 2 {
		return errors.New("not correct")
	}
	a.ip = r[0]
	a.port = r[1]
	return nil
}

type Arg struct {
	Size   int  `flag:"sz" default:"12" usage:"block size"`
	VSize  int  `flag:"vsz" usage:"vblock size"`
	Source addr `flag:"s" default:"127.0.0.1:1001" usage:"destination"`
	Add    *struct {
		Name string `flag:"n"  usage:"add file"`
		B    bool   `flag:"b"`
		High *struct {
			Name string `flag:"n" default:"no" usage:"name"`
		} `flag:"high"`
	} `flag:"add"`
	Delete *struct {
		Name int `flag:"n" usage:"delete file"`
	}
}

var a Arg

func main() {
	var arg Arg
	scli.BuildParser(&arg).Parse()
	fmt.Printf("%+v\n", arg)
}
