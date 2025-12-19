# raytracer

This example renders a simple Cornell box to a `.bmp` file.
Because the language does not support floating points, fixed-point arithmetic is used throughout.

Build the executable like

```
swift run Compile code out.s
clang out.s
```

And run an program like so

```
./a.out > rendering.bmp
```
