# Prerequisite: Haskell

Install [Haskell](https://www.haskell.org/downloads), preferably using Slack.

Haskell `ghc` is needed to compile `GF`.

# Prerequisite: GF

Clone the [Legalese fork of GF](https://github.com/legalese/GF/tree/gflibpaths).

Check out the `gflibpaths` branch.

`make; make install`. Keep an eye on where the libs get installed: you will see three lines like this:

```
Installing [prelude]   /Users/mengwong/Library/Haskell/share/ghc-8.0.2-x86_64/gf-3.9/lib/prelude
Installing [AllTenses] /Users/mengwong/Library/Haskell/share/ghc-8.0.2-x86_64/gf-3.9/lib/alltenses
Installing [Present]   /Users/mengwong/Library/Haskell/share/ghc-8.0.2-x86_64/gf-3.9/lib/present
```

If you previously installed GF from some kind of package, `rm` or `mv` the `gf` binary to `gf-3.8` or `gf-3.9` or whatever, so that `gf` is the one you compiled.

The `gflibpaths` branch of our fork is needed for the following magic:

# `GF_LIB_PATH`

The standard GF install instructions tell you to set, in your `.zshenv` or `.profile`, something like

```
export GF_LIB_PATH=$HOME/Library/Haskell/share/ghc-8.0.2-x86_64/gf-3.9/lib
```

That path corresponds to the three lines above, with `prelude`, `AllTenses`, `Present`.

That's half the library. Now for the other half.

The `gruter` folder should be under your `legalese-compiler` repo, in something like `compiler/sandbox-gf1/201705-may/gruter`

Now append that to the `GF_LIB_PATH`:

```
export GF_LIB_PATH=$HOME/Library/Haskell/share/ghc-8.0.2-x86_64/gf-3.9/lib:$HOME/src/legalese-compiler/sandbox-gf1/201705-may/gruter
```

or whatever is correct in your case.

The standard GF install doesn't grok `:` separated directories in `GF_LIB_PATH`. That's why you need our fork.

