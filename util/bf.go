package main

import "os"

func main() {
	print("#include <stdio.h>\nint main(){ unsigned char mem[30000]; unsigned char *p = mem;\n")
	code := append([]uint8(os.Args[1]), 0)

	for _, c := range code {
		switch c {
		case '+':
			print("(*p)++;")
		case '-':
			print("(*p)--;")
		case '>':
			print("p++;")
		case '<':
			print("p--;")
		case '.':
			print("putchar(*p);")
		case ',':
			print("*p = getchar();")
		case '[':
			print("while(*p){")
		case ']':
			print("}")
		}
	}
	println("return 0;}")
}
