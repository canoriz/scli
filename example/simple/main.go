package main

import (
	"encoding/json"
	"errors"
	"fmt"

	"github.com/canoriz/scli"
)

type Arg struct {
	// Flags(no value) are defined by bool type field
	Help bool `flag:"h" default:"false" usage:"print help"`

	// Arguments(with value) are defined by field of their types
	// define flag, default and usage in struct field's tags
	// flag, default and usage in tag are all optional
	Name  string `flag:"name" default:"you" usage:"your name"`
	Email string `flag:"email" default:"you@example.com" usage:"your email"`

	// subcommands are defined by *struct{...} field
	Add *struct {
		All  bool     `flag:"a" default:"false" usage:"Add all files"`
		File []string `arg:"FILE" usage:"file to be added"`
	} `flag:"add" usage:"Add file contents to the index"`

	Commit *struct{} `flag:"commit" usage:"Record changes to the repository"`

	// subcommands can be nested inside of subcommands!
	Push *struct {
		Origin *struct{} `flag:"origin" usage:"Push to origin"`
	} `flag:"push" usage:"Push to remote repository"`

	Config *struct {
		File scli.File[map[string]any, scli.EnableLiveUpdate]
	}
}

func main() {
	var arg Arg // init a empty argument struct

	parser := scli.BuildParser(&arg).Checker(
		func(a Arg) error {
			if a.Add == nil && a.Commit == nil && a.Push == nil && a.Config == nil {
				return errors.New("at least one subcommand should be specified")
			}
			return nil
		},
	)
	parser.Parse()
	// if Parse() error, program exits

	if arg.Help {
		fmt.Println("help!")
	}
	// fmt.Printf("name: %v, email: %v\n", arg.Name, arg.Email)
	// if arg.Commit != nil {
	// 	fmt.Println("subcommand commit!")
	// }
	// if arg.Add != nil {
	// 	fmt.Println("subcommand add!")
	// 	fmt.Printf("add file: %v\n", arg.Add.File)
	// 	fmt.Printf("add all?: %+v\n", arg.Add.All)
	// }
	// if arg.Push != nil {
	// 	fmt.Println("subcommand push!")
	// 	if arg.Push.Origin != nil {
	// 		fmt.Println("push to origin!")
	// 	}
	// 	fmt.Println("not push to origin!")
	// }
	fmt.Printf("%v\n", prettyPrint(arg))
	if arg.Config != nil {
		fmt.Printf("%v\n", prettyPrint(arg.Config.File.Get()))
		for range arg.Config.File.UpdateEvents() {
			fmt.Printf("%v\n", prettyPrint(arg.Config.File.Get()))
		}
	}
}

func prettyPrint(i interface{}) string {
	s, _ := json.MarshalIndent(i, "", "  ")
	return string(s)
}
