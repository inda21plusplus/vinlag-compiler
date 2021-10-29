# Monkescript-compiler

Monkescript has only 1 type, i64. Strings are constant char* and wont allow basic arithmetic to not shoot yourself in the foot even if they can be returned as a i64 and stored in variables.

Scope does not exist as all variables are declared as 0 and live for the entire function body. This means that you can access a variable before it is set and it will return 0.

Functions can be declared out of order (fuck you c++).

Snake case is also not alowed anywhere as the lexer does not registrer "_"

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
farm getWord(arg) {
	if arg == 2 {
		"Hello"
	} else {
		"World"
	}
}


seed main() {
	a = getWord(1)
	yell("%s %s!\n", a, getWord(2))
	
	getExitcode()
}

farm getExitcode() {
	10*2+2 + { a = 5 a*2+10 }
}
```
Because the lexer is lazy, `;` `.` or `,` will be ignored along with newline and indentation. So semicolon and comma is optional.

A simple for loop 
```
seed main() {
	for x 0..10 {
		yell("x: %d\n" x)
		ifbr x == 5
	}
}
```
break exists for loop, and has 2 options `br` and `ifbr`, `ifbr` takes an argument like `x == 5` and will expand to `if x == 5 { br }`

`while` loops exist too, and has an optinal condition, so `while {` will expand to `while 1 == 1 {`. (`for` loops expand to `while` loops.)

If statments will return the value inside just like `{ ... }` but will return `0` if not met. `else if` and `else` also exist.

```
seed main() {
	x = 3
	
	y = if x == 2 {
		1
	} else if x == 3 {
		2
	} else {
		3
	}
	
	y*2
}
```
A basic prime counter will then look something like 
```
seed main() {
    primes = 0
    
    for n 2..25000 {
        isPrime = 1

        for i 2..1+n/2 {
            if n % i == 0 {
                isPrime = 0
                br
            }
        }
		
        primes = primes + isPrime
    }
    
    primes
}
```
