# scli
To create CLI application use `scli`, all you need is defining an argument struct.

Supported types are `int`, `float64`, `bool`, `string` and their slice types.

`scli` is inspired by Rust's clap, s for struct and simple. `scli` use Go std `flag` package to
create CLI.
## Usage
```go
// define arguments struct
// define flag, default and usage in struct field's tags
// supported types: int, float64, bool, string, []int, []float, []bool, []string
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

	// build parser then parse []string
	// checkArgumentValidity is optional
	scli.Build(&op, checkArgumentValidity).ParseArgs(strings.Split(
		"-h host.com -n 70 -i 1,3,5 -t -nm cindy,david", " ",
	))
	fmt.Printf("parse []string\n%+v\n", op)

	// build parser with checkArgumentValidity checker, then
	// parse command line arguments
	scli.Build(&op, checkArgumentValidity).Parse()
	fmt.Printf("parse command line\n%+v\n", op)
}

// define a post parse checker, return error if none of tcp or udp is enabled,
// Parse() will exit and print usage, ParseArgs() will return this error
func checkArgumentValidity(v *options) error {
	if v.Tcp == v.Udp {
		return errors.New("exactly one of tcp or udp must be enabled")
	}
	return nil
}
```

Run `./main --help`
```
parse []string
{Host:host.com Port:80 Num:70 Ratio:3.14159 Tcp:true Udp:false Names:[cindy david] Index:[1 3 5]}
Usage of ./example:
  -Ratio float
         (default 3.14159)
  -h string
        hostname
  -i value
         []int, input by int[,int]... (default 1,2,3)
  -n int
    
  -nm value
        names []string, input by string[,string]... (default alice,bob)
  -p string
        port (default "80")
  -t    use tcp
  -u    use udp
```

## Notes for `panic`
To parse arguments, there are 2 stages.
1. `Build(*Arg) error)` to build a parser
2. `Parse()`, `ParseArgs(..)`to parse arguments

Stage 1 is like compile a `struct{..}` to `parser`, `Build` will check
struct field's type and tags, if error found in type and tags,
`Build` panics and tells the detailed error. `Build` is considered
as compile time. At Stage 2, `ParseArgs` never panics, if parse
failed, error is returned. `Parse`, used as the root parser of
command arguments, if failed, program exits. `Parse`, `ParseArgs` is considered
runtime.

