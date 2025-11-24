# brainf*ck

This is a [brainf*ck](https://en.wikipedia.org/wiki/Brainfuck#:~:text=Brainfuck%20is%20an%20esoteric%20programming,Esoteric%2C%20imperative%2C%20structured) interpreter. It reads a program from stdin. There is no input command in the language, since stdin is used for reading code.

Build the executable like

```
swift run Compile code out.s
clang out.s
```

And run an program like so

```
cat hello_world.bf | ./a.out
```

In that case, the output looks like

```
Hello, world!
```

