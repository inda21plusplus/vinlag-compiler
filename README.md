# Monkescript-compiler

Monkescript has only 1 type, i64. Strings are constant char* and wont allow basic arithmetic to not shoot yourself in the foot even if they can be returned as a i64 and stored in variables.

Scope does not exist as all variables are declared as 0 and live for the entire function body. This means that you can access a variable before it is set and it will return 0.

Functions can be declared out of order (fuck you c++)

Hello world looks like this:
1. `seed` for main (and `farm` for function)
2. `yell` instead of printf
3. The last variable is returned as the exitcode

```
seed main() {
	yell("Hello World!\n")
	
	42
}
```
A more complicated example
```
farm getword(arg) {
	if arg == 2 {
		"Hello"
	} else {
		"World"
	}
}


seed main() {
	a = getword(1)
	yell("%s %s!\n", a, getword(2))
	
	getexitcode()
}

farm getexitcode() {
	10*2+2 + { a = 5 a*2+10 }
}
```
Because the lexer is lazy, `;` `.` or `,` will be ignored along with newline and indentation. So semicolon and comma is optional.
