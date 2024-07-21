package scli

import (
	"fmt"
	"strconv"
	"strings"
)

func makeUsageText(viewNameChain string, cmd cmdInfo) string {

	optionUsageList := makeOptionUsageList(cmd)
	argUsageList := makeArgUsageList(cmd)
	subcmdUsageList := makeSubcmdUsageList(cmd)

	// arguments always have a --help option
	usage := fmt.Sprintf("Usage: %s [OPTIONS]", viewNameChain)
	if cmd.HasArgs() {
		for _, a := range cmd.args {
			usage = fmt.Sprintf("%s <%s>", usage, a.cliName)
		}
	}
	if cmd.HasSubcmds() {
		usage = fmt.Sprintf("%s [COMMAND]", usage)
	}
	usage = usage + "\n"

	if len(argUsageList) > 0 {
		usage += fmt.Sprintf(
			"\nArguments:\n%s\n", strings.Join(fmap(argUsageList, shiftFour), "\n"),
		)
	}
	if len(subcmdUsageList) > 0 {
		usage = fmt.Sprintf("Usage: %s [OPTIONS] [COMMAND]\n", viewNameChain)
		usage += fmt.Sprintf(
			"\nCommands:\n%s\n", strings.Join(fmap(subcmdUsageList, shiftFour), "\n"),
		)
	}

	// arg always have --help, thus not empty
	usage += fmt.Sprintf("\nOptions:\n%s\n", strings.Join(fmap(optionUsageList, shiftFour), "\n"))
	if len(subcmdUsageList) > 0 {
		usage += fmt.Sprintf(
			"\nRun `%s [COMMAND] -help` to print the help message of COMMAND\n\n",
			viewNameChain,
		)
	}
	return usage
}

func makeOptionUsageList(cmd cmdInfo) []string {
	const helpArg = "-help, --help"
	const boolArgFmt = "--%s, --/%s"
	const generalArgFmt = "--%s <%s>"

	optionUsageList := []string{}
	maxArgLength := len(helpArg)
	for _, a := range sortMap(cmd.options) {
		argName := a.key
		argInfo := a.val
		unifiedUsage := argName
		if argInfo.usage != "" {
			unifiedUsage = a.val.usage
		}
		maxArgLength = maxInt(
			maxArgLength,
			func() int {
				if a.val.ty == boolArg {
					return len(fmt.Sprintf(boolArgFmt, argName, argName))
				}
				return len(fmt.Sprintf(generalArgFmt, argName, unifiedUsage))
			}(),
		)
	}
	optionUsageList = append(
		optionUsageList,
		fmt.Sprintf(
			"%s  %s",
			appendSpacesToLength(helpArg, maxArgLength), "print this message",
		),
	)
	for _, a := range sortMap(cmd.options) {
		info := a.val
		argName := a.key
		argUsage := func() string {
			unifiedUsage := argName
			if info.usage != "" {
				unifiedUsage = info.usage
			}
			if info.ty == boolArg {
				return fmt.Sprintf(
					"%s  %s",
					appendSpacesToLength(
						fmt.Sprintf(boolArgFmt, argName, argName),
						maxArgLength,
					),
					fmt.Sprintf("set [%s] to true / false",
						unifiedUsage,
					),
				)
			}
			return appendSpacesToLength(
				fmt.Sprintf(generalArgFmt, argName, unifiedUsage),
				maxArgLength,
			)
		}()

		if info.defaultVal != nil {
			defaultText := func() string {
				if info.ty == boolArg {
					// default value of bool type can be T, F, 0, 1, true, false
					// for consistency, only print true/falue in help message
					// default value among {T, 1, true} will print true
					// {F, 0, false} will print false
					// all default value is verified before, always success
					b, _ := strconv.ParseBool(*info.defaultVal)
					return fmt.Sprintf(`[default: %v]`, b)
				}
				return fmt.Sprintf(`[default: "%s"]`, *info.defaultVal)
			}()
			argUsage = fmt.Sprintf("%s  %s", argUsage, defaultText)
		} else if info.example != nil {
			// if argument has default value, users can learn how to
			// input this argument by reading the default value, so we
			// don't need to print an example.
			argUsage = fmt.Sprintf(
				"%s  %s",
				argUsage,
				fmt.Sprintf(`[example: "%s"]`, *info.example),
			)
		}

		optionUsageList = append(optionUsageList, argUsage)
	}
	return optionUsageList
}

func makeArgUsageList(cmd cmdInfo) []string {
	argsUsageList := []string{}
	maxArgLength := 0
	for _, a := range cmd.args {
		maxArgLength = maxInt(
			maxArgLength,
			2+len(a.cliName), // add 2 for <> or [] around a.cliName
		)
	}
	for _, a := range cmd.args {
		argUsage := fmt.Sprintf("<%s>", a.cliName)
		if a.usage != "" {
			argUsage = fmt.Sprintf(
				"%s  %s",
				appendSpacesToLength(argUsage, maxArgLength),
				a.usage,
			)
		}
		argsUsageList = append(argsUsageList, argUsage)
	}
	return argsUsageList
}

func makeSubcmdUsageList(cmd cmdInfo) []string {
	subcmdUsageList := []string{}
	maxCmdLength := 0
	for _, c := range sortMap(cmd.subcmds) {
		maxCmdLength = maxInt(maxCmdLength, len(c.key))
	}
	for _, c := range sortMap(cmd.subcmds) {
		cmd := c.key
		info := c.val
		cmdUsage := cmd
		if info.usage != "" {
			cmdUsage = fmt.Sprintf(
				"%s  %s",
				appendSpacesToLength(cmd, maxCmdLength),
				info.usage,
			)
		}
		subcmdUsageList = append(subcmdUsageList, cmdUsage)
	}
	return subcmdUsageList
}

func shiftFour(s string) string {
	const fourSpace = "    "
	return fourSpace + s
}

func fmap(ss []string, f func(string) string) []string {
	for i, s := range ss {
		ss[i] = f(s)
	}
	return ss
}

func appendSpacesToLength(s string, toLength int) string {
	needSpace := toLength - len(s)
	for i := 0; i < needSpace; i++ {
		s += " "
	}
	return s
}
