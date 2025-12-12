# learn-compilers

This is a compiler written for a toy C-like programming language that I call `C??`. The compiler can perform basic optimizations like constant propagation, inlining, and dead code elimination.

## The C?? language

A values in C?? have one of two types: `int` (64-bit signed integers) and `str` (a reference to a mutable array of bytes).

Like in C, you can create integers and strings with literals, such as `32` and `"hello"`. There are also functions in C??. For example, you can use `foo(32, "hello")` to call a function `foo` with an integer argument followed by a string argument.

You can define variables by specifying the variable name, type, and initial value. Variables can be used in place of literals or other expressions.

```
x: int = 32
y: str = "hello"

// Call a function bar with the result of foo.
foo(x, y)
```

Unlike in C, there are no arithmetic operators to manipulate numbers. Instead, you can use functions like `add` and `mul`, for example `add(3, mul(5, 5))` is how you express `3 + 5*5`.

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

There are various built-ins for comparisons like `not`, `eq`, `lt`, `gt`. Unlike in C, there is no equivalent `else` or `else if` syntax.

You can define a function with the `fn` keyword, and can return a value with the special `return!` statement, which looks similar to a function itself:

```
fn main() -> int {
    return!(addmul(2, 3, 4)) // returns 2*3 + 4 = 10
}

fn addmul(x: int, y: int, z: int) -> int {
  return!(add(mul(x, y), z))
}
```

Functions don't always need a return value. In this case, omit the `-> ...` from the definition, and pass no arguments to `return!` if you need to issue an early return:

```
fn foo(x: int) {
  if? (x) {
    println("argument is true")
    return!() // Return early
  }
  println("argument is false")
  // No need to return!() at the end of the function body.
}
```

To work with strings, you can use `str_alloc`, `str_free`, `str_get` and `str_set`. Compile-time constant strings cannot be modified by `str_set` (if you do it anyway, behavior is undefined).

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

Unlike in C, functions in C?? can be overloaded by definining versions of the same function with different argument type signatures. For example,

```
fn foo(x: int) {
  println("version 1")
}

fn foo(x: str) {
  println("version 2")
}

fn foo(x: int, y: str) {
  println("version 3")
}

fn main() {
  foo(3, "hi") // will print "version 3"
}
```

This is used in the standard library. For example, the `not` function can take both an `int` or a `str`.

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

If you run the `Compile` command with an output path that ends with `.cfg.txt`, then instead of assembly, you will get a file containing a description of the control flow graph (the main intermediate representation used by the compiler).

## How the compiler works

When you compile code, the following steps are performed:

 1. The code is parsed using a context-free grammar [defined here](Sources/LearnCompilers/SimpleGrammar.swift). This is where syntax errors are caught.
 2. The grammar match tree is converted to an abstract syntax tree (AST) [defined here](Sources/LearnCompilers/AST.swift).
 3. The AST is "decorated" by resolving scopes, function references, and variables [here](Sources/LearnCompilers/ASTDecorations.swift). This is where errors are raised for unknown references, and also where type errors are raised. The only remaining possible compile errors are missing returns, since these are only determined after the control flow graph is built.
 4. The AST is converted into a control flow graph (CFG) [defined here](Sources/LearnCompilers/CFG.swift), where if-statements, while loops, and function calls are turned into simpler abstract instructions with edges in the graph defining control flow. This step introduces temporary variables to deal with nested expressions.
 5. The variables in the CFG are transformed into static-single assignment (SSA) variables, where each assignment defines a separate version of the variable. Special `phi` instructions are inserted to merge different versions of variables from different predecessor nodes (when necessary).
 6. Various [optimization passes](Sources/LearnCompilers/Optimization.swift) are performed on the SSA'd CFG to reduce the number of instructions and eliminate redundant or dead code.
 7. Missing returns are detected. This is done late so that optimization passes can help determine if a return is ever missing (e.g. an infinite loop will prevent the end of a function from ever being reached, and it might require some work to prove that the loop never terminates).
 8. The backend converts the SSA'd CFG into assembly code. This includes using [parallel moves](Sources/LearnCompilers/ParallelMove.swift) to implement phi instructions, and [liveness](Sources/LearnCompilers/Liveness.swift) with [graph coloring](Sources/LearnCompilers/Color.swift) to allocate registers and stack slots for variables.

As an alternative to step 8, we can also use an [Interpreter](Sources/LearnCompilers/Interpreter.swift) to run the CFG directly (without converting it into machine code). The interpreter is very inefficient compared to a compiled binary, but it's nice for testing the CFG itself.

The language provides a few built-in functions for string and integer operations. The backend or interpreter will typically handle these in some special way that doesn't look like normal standard function call. There is also a [standard library](Sources/LearnCompilers/StandardLibrary.swift) which contains code directly written in `C??`. These functions are treated exactly like user-provided functions.

## Example of CFG and optimizations

This section gives an example of the compiler's intermediate representation and the kinds of optimizations it can perform.

We will study the following simple program, which reads a byte `x` from standard input, computes `x*2 + 3`, and returns the result from the `main` function. The program has some superfluous if-statements that will help us see how branches are implemented in the SSA.

```
fn mul_add(x: int, y: int, z: int) -> int {
  result: int = x
  if? (not(eq(y, 1))) {
    result = mul(result, y)
  }
  if? (not(eq(z, 0))) {
    result = add(result, z)
  }
  return!(result)
}

fn main() -> int {
  x: int = getc()
  return!(mul_add(x, 2, 3))
}
```

When we compile this with no optimizations, we get [a large control flow graph](examples/mul_add/unoptimized.cfg.txt) with a lot of interesting features that we can unpack.

For example, let's look at the first if-statement in `mul_add`

```
if? (not(eq(y, 1))) {
  result = mul(result, y)
}
```

It is implemented as an `eq` call stored into a temporary variable `tmp[0]`, followed by a `not` call stored into another temporary variable `tmp[1]`, followed by a `.check` instruction which branches to two different places depending on the condition; if the comparison is true, we continue to `node3`; otherwise we jump to `node2`. In `node3`, we create a new version of the `result` variable and then jump to `node2`. At the exit point of the if-statement (`node2`), we merge the result of the if-statement, which was version 0 in the false path and version 1 in the true path:

```
  .callAndStore(
    target=SSAVariable(Variable(name=tmp[0], type=int, isArgument=false, pos=addmul:3:12), version=0)
    fn=Function(name=eq, args=[int, int] ret=Optional(int)),
    args=[
      .variable(SSAVariable(Variable(name=y, type=int, isArgument=true, pos=addmul:1:19), version=0)),
      .constInt(1)
    ]
  )
  .callAndStore(
    target=SSAVariable(Variable(name=tmp[1], type=int, isArgument=false, pos=addmul:3:8), version=0)
    fn=Function(name=not, args=[int] ret=Optional(int)),
    args=[
      .variable(SSAVariable(Variable(name=tmp[0], type=int, isArgument=false, pos=addmul:3:12), version=0))
    ]
  )
  .check(.variable(SSAVariable(Variable(name=tmp[1], type=int, isArgument=false, pos=addmul:3:8), version=0)))
  branch true=node2 true=node3

node3:
  .callAndStore(
    target=SSAVariable(Variable(name=tmp[2], type=int, isArgument=false, pos=addmul:4:14), version=0)
    fn=Function(name=mul, args=[int, int] ret=Optional(int)),
    args=[
      .variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=addmul:2:3), version=0)),
      .variable(SSAVariable(Variable(name=y, type=int, isArgument=true, pos=addmul:1:19), version=0))
    ]
  )
  .copy(
    target=SSAVariable(Variable(name=result, type=int, isArgument=false, pos=addmul:2:3), version=1),
    source=.variable(SSAVariable(Variable(name=tmp[2], type=int, isArgument=false, pos=addmul:4:14), version=0))
  )
  goto node2

node2:
  .phi(
    SSAVariable(Variable(name=result, type=int, isArgument=false, pos=addmul:2:3), version=2),
    branches=[
      (key: Node(0), value: .variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=addmul:2:3), version=0))),
      (key: Node(3), value: .variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=addmul:2:3), version=1)))
    ]
  )
```

The implementation of the main function is pretty straightforward. Here, we call `getc` and first assign it to a temporary variable `tmp[6]`, then copy `tmp[6]` into `x`. We then call `mul_add` and assign its result to `tmp[7]`. Return values are typically stored in a special `<return value>` variable; this way, every function has at most one `.returnValue` instruction which combines different return sites using phi function(s).

```
node6:
  .callAndStore(
    target=SSAVariable(Variable(name=tmp[6], type=int, isArgument=false, pos=addmul:13:12), version=0)
    fn=Function(name=getc, args=[] ret=Optional(int)),
    args=[
    ]
  )
  .copy(
    target=SSAVariable(Variable(name=x, type=int, isArgument=false, pos=addmul:13:3), version=0),
    source=.variable(SSAVariable(Variable(name=tmp[6], type=int, isArgument=false, pos=addmul:13:12), version=0))
  )
  .callAndStore(
    target=SSAVariable(Variable(name=tmp[7], type=int, isArgument=false, pos=addmul:14:11), version=0)
    fn=Function(name=mul_add, args=[int, int, int] ret=Optional(int)),
    args=[
      .variable(SSAVariable(Variable(name=x, type=int, isArgument=false, pos=addmul:13:3), version=0)),
      .constInt(2),
      .constInt(3)
    ]
  )
  .copy(
    target=SSAVariable(Variable(name=<return value>, type=int, isArgument=false, pos=addmul:12:10), version=0),
    source=.variable(SSAVariable(Variable(name=tmp[7], type=int, isArgument=false, pos=addmul:14:11), version=0))
  )
  goto node7

node7:
  .returnValue(.variable(SSAVariable(Variable(name=<return value>, type=int, isArgument=false, pos=addmul:12:10), version=0)))
```

When we build this code with optimizations enabled, we get a much simpler function, with only four instructions:

```
CFG(
  fn main {
    node6:
      .callAndStore(
        target=SSAVariable(Variable(name=x, type=int, isArgument=false, pos=addmul:13:3), version=0)
        fn=Function(name=getc, args=[] ret=Optional(int)),
        args=[
        ]
      )
      .callAndStore(
        target=SSAVariable(Variable(name=result, type=int, isArgument=false, pos=addmul:2:3), version=1)
        fn=Function(name=mul, args=[int, int] ret=Optional(int)),
        args=[
          .variable(SSAVariable(Variable(name=x, type=int, isArgument=false, pos=addmul:13:3), version=0)),
          .constInt(2)
        ]
      )
      .callAndStore(
        target=SSAVariable(Variable(name=result, type=int, isArgument=false, pos=addmul:2:3), version=3)
        fn=Function(name=add, args=[int, int] ret=Optional(int)),
        args=[
          .variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=addmul:2:3), version=1)),
          .constInt(3)
        ]
      )
      .returnValue(.variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=addmul:2:3), version=3)))
  }
)
```

Notably, the compiler inlined `mul_add()`, allowing for further optimizations. Since we call `mul_add` with a constant value of `y` and `z`, the compiler was able to determine the outcome of both if-statements, allowing it to remove branches. After eliminating branches and unreachable code paths, it removed all of the phi functions, since they aren't needed when there's only one possible control flow path. Finally, you'll note that there's no longer a special `<return value>` variable, or any temporary variables. Whenever a value is copied into a variable, the variable can be eliminated, since the compiler can replace all future uses of the variable with its assigned value.

You can also see the AArch64 assembly of the [unoptimized](examples/mul_add/unoptimized.s) and [optimized](examples/mul_add/optimized.s) CFGs, respectively.
