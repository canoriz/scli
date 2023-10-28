# scli
Defining an argument struct and calling `scli` is all you need to create a CLI application.

`scli` supports `int`, `float64`, `bool`, `string` or any type implemented `scli.Parse`
arguments and arbitrary layer of subcommands with these types of arguments.

Any argument/subcommand `scli` supports can have a configurable CLI argument name, default value and
usage message.

`scli` can directly parse arguments from CLI, or from `string`. You can test your configuration
by parse `string`.

## Usage
```go
// this is same as example/simple/main.go
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
```

Run `go run example/simple/main.go --help`
```
Usage: /tmp/go-build2692279630/b001/exe/main [OPTIONS] [COMMAND]

Commands:
    Div
    add
        n1+n2
    mul
        n1*n2
    sub
        n1-n2
Run `/tmp/go-build2692279630/b001/exe/main [COMMAND] -help` to see command help message

Options:
    -help, --help
        show this help message
    --p ([print result] true) --/p ([print result] false)
        [default: true]
```

## Notes for `panic`
To parse arguments, there are 2 stages.
1. `BuildParser(*struct{...}) error)` to build a parser
2. `Parse()`, `ParseArgs(...)`to parse arguments

`BuildParser` will check struct field's type and tags,
if error found in type and tags, `BuildParser` panics and tells
the detailed error. You can think `BuildParser` compile `struct{...}` to
corresponding `Parser`, and compile may error.

`Parse()`, `ParseArgs(...)` parse input arguments from CLI or `[]string`,
they never panics, if parse fails, error is returned.

`Parse()` do a bit more than `ParseArgs(...)`. If `Parse()` meets error,
`Parse()` ends program, show parse errors and program USAGE.

If a custom type `Custom` is in arguments struct, and the custom type's
`Custom.FromString(string)` method may `panic`.
If `FromString` panic, `Parse()` and `ParseArg()` will `panic`.
