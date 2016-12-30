# Advent of Code

This includes my Haskell solutions to [Advent of Code][1]. Typically they are
compiled with:

```
stack runghc 1.hs < 1.input
```

or for more cpu-intensive problems:

```
stack ghc 1.hs && ./1 < 1.input
```

I'm sure there's a better way to do this, but it's all pretty one-offy. Some
problems were solved by hand (so you might find some `undefined`s that won't
evaluate). Some solutions commented out the first part and only print the second
part for performance reasons.

It should be noted that I used this to actually learn how to write real haskell
code. There are a lot of weird patterns and spots that could benefit from tools
that already exist, but I haven't gotten around to learning them.

[1]: https://adventofcode.com/
