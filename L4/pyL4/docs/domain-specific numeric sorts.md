Look at `SAFE.py`. There are 3 special kinds of numbers:

1. `Shares` - counts of shares. This is a kind of number derived from `Nat`. There is also the positive subsort `PosShares`.

2. `$` - amounts of money. This is a kind of number derived from `NonnegReal`. There is also the positive subsort `Pos$`.
 
3. `$/Share` - share prices. This is the set of fractions representable as a `$` divided by a `PosShares`. It is *not* technically a sort duplicate.

In pyL4, `Shares` is a shorthand name for the sort `(Dup Nat "Shares")`; that it is the same string as the second argument to the sort constructor `Dup` is recommended, but not required.

**Note**: We'll see that `Dup` is not *really* an appropriate name for this sort constructor, since the sorts it generates do not mix as freely with the other numeric types; this requires some more explanation, but as a start: consider that we don't want to automatically bring into existence fractional shares.

_TODO_: Properties of domain-specific numeric sorts.

