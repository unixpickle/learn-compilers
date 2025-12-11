# learn-compilers

This is a compiler written for a toy C-like programming language that I call `C??`. The compiler can perform basic optimizations like constant propagation, inlining, and dead code elimination.

## The C?? language

You can define a function with the `fn` keyword, and can return a value with the special `return!` syntax. The syntax for calling functions is pretty straightforward:

```
fn main() -> int {
    return!(addmul(2, 3, 4)) // returns 2*3 + 4 = 10
}

fn addmul(x: int, y: int, z: int) -> int {
  return!(add(mul(x, y), z))
}
```

You can define variables by specifying the variable name, type, and initial value:

```
x: int = 3
y: str = "hello world"
```

You can manipulate values with function calls, for example

```
x: int = 3
y: int = 7
z: int = add(x, mul(y, 2)) // 3 + 7*2
```

You can check conditions or do loops with the `if?` and `while?` statements:

```
x: int = some_function()

if? (eq(x, 3)) {
  print("x is 3")
}

// Loop while x < 3
while? (lt(x, 3)) {
  x = add(x, 1)
}
```

There are various built-ins for comparisons like `not`, `eq`, `lt`, `gt`.

The supported datatypes are `int` (64-bit signed integer) and `str` (a dynamic array of bytes). To work with strings, you can use `str_alloc`, `str_free`, `str_get` and `str_set`. Compile-time constant strings cannot be modified by `str_set`.

For example:

```
a: int = str_get("a", 0)
s: str = str_alloc(3)
str_set(s, 0, a)
str_set(s, 1, a)
str_set(s, 2, a)
println(s) // prints "aaa"
str_free(s)
```

Strings are references; to copy them, you can explicitly use `str_copy`:

```
a: str = str_copy("abcd")
b: str = a
str_set(a, 0, str_get(a, 1))
println(b) // prints "bbcd"
str_free(a)

// This would be invalid:
// str_free(b)
```

## Running code

To run `C??` code, you can either compile the code to a supported architecture (currently, only AArch64 for M-series Macs), or run the compiler's intermediate representation directly inside an interpreter.

For example, let's run the [integer factorization example](examples/factorize/code). We can run it in the interpreter like so

```
swift run Interpret examples/factorize/code
```

Alternatively, we could compile the code like so:

```
swift run Compile examples/factorize/code out.s
clang out.s
./a.out
```

If you run the `Compile` command with an output path that ends with `.cfg.txt`, then you will instead get a file containing a description of the control flow graph.

## How the compiler works

When you compile code, the following steps are performed:

 1. The code is parsed using a context-free grammar [defined here](Sources/LearnCompilers/SimpleGrammar.swift). This is where syntax errors are caught.
 2. The context-free grammar match is converted to an abstract syntax tree (AST) [defined here](Sources/LearnCompilers/AST.swift).
 3. The AST is "decorated" by resolving scopes, function references, and variables [here](Sources/LearnCompilers/ASTDecorations.swift).
 4. The AST is converted into a control flow graph (CFG) [defined here](Sources/LearnCompilers/CFG.swift), where if statements, while loops, and function calls are turned into simpler abstract instructions with edges in the graph defining control flow. This step introduces temporary variables to deal with nested expressions.
 5. The variables in the CFG are turned into SSA variables, where each definition is a separate version of the variable. Special `phi` instructions are inserted into blocks to merge definitions of variables from separate predecessor nodes when necessary.
 6. Various [optimization passes](Sources/LearnCompilers/Optimization.swift) are performed on the SSA'd CFG to reduce the number of instructions and eliminate redundant or dead code.
 7. Missing returns are detected. This is done late so that optimization passes can help determine if a return is ever missing (e.g. an infinite loop will prevent the end of a function from ever being reached, and it might require some work to prove that the loop never terminates).
 8. The backend converts the SSA'd CFG into assembly code. This includes using [parallel moves](Sources/LearnCompilers/ParallelMove.swift) to implement phi instructions, and [liveness](Sources/LearnCompilers/Liveness.swift) with [graph coloring](Sources/LearnCompilers/Color.swift) to allocate registers and stack slots for variables.

As an alternative to 8, we can also use an [Interpreter](Sources/LearnCompilers/Interpreter.swift) to run the CFG directly, without converting it into machine code. This is very inefficient, but good for quick testing.

The language provides a few built-ins for string and integer operations. The backend or interpreter will typically handle these in some special way that doesn't look like a standard function call. There is also a [standard library](Sources/LearnCompilers/StandardLibrary.swift) which contains code directly written in `C??`. These are treated exactly like user-provided functions, and only exist for convenience.
