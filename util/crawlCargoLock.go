package main

import (
	"os"
	"os/exec"
	"strings"
)

func main() {
	help := "First arg is path to 'Cargo.lock'. that will output to 'out.cloctables', then run again with 'out.cloctables' as the first arg."
	println(help)

	path := "./Cargo.lock"
	if len(os.Args) > 1 {
		path = os.Args[1]
	}

	if strings.HasSuffix(path, ".lock") {
		// this takes a thousand years so i save in a file for testing faster
		out := run_cloc_based_on_lock_file(path)
		outPath := "out.cloctables"
		if os.WriteFile(outPath, []byte(out), 0666) != nil {
			print(out)
		} else {
			println("Wrote output to " + outPath)
		}
	} else if strings.HasSuffix(path, ".cloctables") {
		content := string(unwrap(os.ReadFile(path)))
		println(content)
	}
}

func run_cloc_based_on_lock_file(path string) string {
	content := string(unwrap(os.ReadFile(path)))
	lines := strings.Split(content, "\n")

	name := ""
	version := ""
	source := ""
	output := ""
	for i := 0; i < len(lines); {
		if lines[i] == "[[package]]" {
			for i < len(lines) {
				parts := strings.Split(lines[i], " = ")
				if parts[0] == "name" {
					name = parts[1][1 : len(parts[1])-1]
				}
				if parts[0] == "version" {
					version = parts[1][1 : len(parts[1])-1]
				}
				if parts[0] == "source" {
					source = parts[1][1 : len(parts[1])-1]
					output += run_cloc(name, source, version)
				}
				i += 1
			}
		}
		i += 1
	}
	return output
}

func run_cloc(name string, source string, version string) string {
	parts := strings.Split(source, "+")
	if len(parts) != 2 {
		panic("malformed package source")
	}
	kind, url := parts[0], parts[1]
	path := ""
	if kind == "registry" && url == "https://github.com/rust-lang/crates.io-index" {
		path = "/Users/luke/.cargo/registry/src/index.crates.io-6f17d22bba15001f/" + name + "-" + version
	} else if kind == "git" {
		// path = "~/.cargo/git/checkouts/" + name + "-" + version
	}
	out := ""
	if path != "" {
		out += name + " " + version + "\n"
		cmd := exec.Command("npx", "cloc", path)
		out += string(unwrap(cmd.CombinedOutput()))
	}
	print(out)
	return out
}

func unwrap[T any](value T, err error) T {
	if err != nil {
		panic(err)
	}
	return value
}
