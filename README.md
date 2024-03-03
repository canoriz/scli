# scli
Build a command line argument parser by defining a struct

# Example
This example is in example/simple/main.go
```go
// define flag, default and usage in struct field's tags
type Arg struct {
	// Flags(no value) are defined by bool type field
	Help bool `flag:"h" default:"false" usage:"print help"`

	// Arguments(with value) are defined by field of their types
	Name  string `flag:"name" default:"you" usage:"your name"`
	Email string `flag:"email" default:"you@example.com" usage:"your email"`

	// subcommands are defined by *struct{...} field
	Add *struct {
		All  bool   `flag:"a" default:"false" usage:"Add all files"`
		File string `flag:"f" usage:"file to be added"`
	} `flag:"add" usage:"Add file contents to the index"`
	Commit *struct{} `flag:"commit" usage:"Record changes to the repository"`

	// subcommands can be nested inside of subcommands!
	Push *struct {
		Origin *struct{} `flag:"origin" usage:"Push to origin"`
	} `flag:"push" usage:"Push to remote repository"`
}

func main() {
	var arg Arg // init a empty argument struct

	parser := scli.BuildParser(&arg)
	parser.Parse()
    fmt.Printf("%v\n", prettyPrint(arg))
}
```

Below are some examples of input CLI arguments and parsed values of ```arg```.
```bash
$ ./mygit 
{
  "Help": false,
  "Name": "you",
  "Email": "you@example.com",
  "Add": null,
  "Commit": null,
  "Push": null
}

$ ./mygit --name canoriz
{
  "Help": false,
  "Name": "canoriz",
  "Email": "you@example.com",
  "Add": null,
  "Commit": null,
  "Push": null
}

$ ./mygit --name canoriz add -a -f source-code
{
  "Help": false,
  "Name": "canoriz",
  "Email": "you@example.com",
  "Add": {
    "All": true,
    "File": "source-code"
  },
  "Commit": null,
  "Push": null
}

$ ./mygit --name canoriz commit
{
  "Help": false,
  "Name": "canoriz",
  "Email": "you@example.com",
  "Add": null,
  "Commit": {},
  "Push": null
}

$ ./mygit --name canoriz push origin
{
  "Help": false,
  "Name": "canoriz",
  "Email": "you@example.com",
  "Add": null,
  "Commit": null,
  "Push": {
    "Origin": {}
  }
}

$ ./mygit --name canoriz push
{
  "Help": false,
  "Name": "canoriz",
  "Email": "you@example.com",
  "Add": null,
  "Commit": null,
  "Push": {
    "Origin": null
  }
}

$ ./mygit --name canoriz add
error: argument "--f" is required in "./mygit add" but not provided

The usage is:
Usage: ./mygit add [OPTIONS]

Options:
    -help, --help           print this message
    --a, --/a               set [Add all files] to true / false  [default: false]
    --f <file to be added>

$ ./mygit --help
Usage: ./mygit [OPTIONS] [COMMAND]

Commands:
    add     Add file contents to the index
    commit  Record changes to the repository
    push    Push to remote repository

Options:
    -help, --help         print this message
    --email <your email>  [default: "you@example.com"]
    --h, --/h             set [print help] to true / false  [default: false]
    --name <your name>    [default: "you"]

Run `./mygit [COMMAND] -help` to print the help message of COMMAND

$ ./mygit add --help
Usage: ./mygit add [OPTIONS]

Options:
    -help, --help           print this message
    --a, --/a               set [Add all files] to true / false  [default: false]
    --f <file to be added>
```


# Feature

## Any type of argument
Supports arguments of type `int`, `float64`, `bool`, `string`, `[]int`,
 `[]float`, `[]bool`, `[]string`, or any type `T` that `*T`
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
