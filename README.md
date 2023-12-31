# scli
Defining an argument struct and calling `scli` is all you need to create a CLI application.

# Feature

## Any type of argument
Supports arguments of type `int`, `float64`, `bool`, `string` or any type
implemented `scli.Parse`.

For example, if `Addr {string; string}` struct defines a `(*Addr).FromString(string) error`
method parsing `127.0.0.1:80` to `Addr {"127.0.0.1", "80"}`, CLI can have a type `Addr`
argument.
- `./my-program -addr 127.0.0.1:80`

## Arbitrary nested layer of subcommands
Supports defining CLI program like below, where `remote`, `local` are layer 1 subcommands,
`add`, `remove` are layer 2 subcommands.
- `./my-program remote add -name Alice`
- `./my-program local add -name Bob`
- `./my-program remote remove -name Cindy`

Any argument/subcommand can have a configurable CLI argument name and usage message.

## Default values
Supports default value for any type of arguments by simply add a `default` tag, including custom types.
- ```
  type Arg struct {
      addr Addr `default:"127.0.0.1:80"`
  }
  ```

# Example
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
Usage: ./simple [OPTIONS] [COMMAND]

Commands:
    Div
    add  n1+n2
    mul  n1*n2
    sub  n1-n2

Options:
    -help, --help  print this message
    --p, --/p      set [print result] to true / false  [default: true]

Run `./simple [COMMAND] -help` to print the help message of COMMAND
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
