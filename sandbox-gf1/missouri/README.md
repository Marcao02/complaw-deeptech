# About

In this exercise we attempt to model [MO Rev Stat § 233.285 (2015)](http://law.justia.com/codes/missouri/2015/title-xiv/chapter-233/section-233.285/) which was chosen as a good example of a law that
- is very long
- modifies prior law
- is conscious of modifying prior law
- contains constitutive rules
- contains regulatory rules
- contains one or two elements that might be considered mistakes
- contains one or two elements that might be considered ambiguities
- contains repetitive patterns that appear to "unroll the loop"
- uses "shall" in various senses, sometimes to obligate the parties, sometimes to determine the reading of the text

Analyzing the text, one might conjecture that a formalization of the logic of the text would involve:
- temporal modals --- to describe events and fluents in time
- deontic modals --- to describe the obligations of parties, aka regulative rules
- evaluation of intensional predicates to an extensional determination --- to support constitutive rules
- first-order logic --- to assign truth or false values to combinations of elements within the text

See <missouri.org> for more detail on the text itself.

# Usage

```
20170824-12:05:14 mengwong@venice2:~/non-db-src/l/compiler/sandbox-gf1/missouri% gf MissouriIEng.gf

         *  *  *
      *           *
    *               *
   *
   *
   *        * * * * * *
   *        *         *
    *       * * * *  *
      *     *      *
         *  *  *

This is GF version 3.9.
Built on darwin/x86_64 with ghc-8.0, flags: interrupt
License: see help -license.

linking ... OK

Languages: MissouriIEng
Missouri> l mkReactionRule repealing MustNot abate
the repealing of the section and laws repealed by this law shall not have the effect of abating , nullifying , suspending or vitiating any public road district incorporated prior to the taking-effect of this law or any proceeding by any such public road district .

9 msec
```

# Installation

To run `gf MissouriIEng.gf` you may need to install a tweaked GF.

## Prerequisite: Haskell

Install [Haskell](https://www.haskell.org/downloads), preferably using Slack.

Haskell `ghc` is needed to compile `GF`.

## Prerequisite: GF

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

## `GF_LIB_PATH`

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

