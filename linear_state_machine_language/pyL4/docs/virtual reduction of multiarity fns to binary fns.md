This little document records the approach we use in pyL4 for typechecking unbounded-arity functions, which at the moment fall into two categories:

1. `ASSOCIATIVE_OPS = {'or', 'and', 'min', 'max', '+', '*'}`
2. `CHAIN_PREDS = {'≤', '≥', '<', '>', '=='}`

We "virtually" reduce applications of arity > 2 to multiple applications of arity 2.

For example, `(0 + 2 + 0)` will have type `PosInt`.

"Virtual" just means that we don't literally construct the term `((0 + 2) + 0)`; so don't look for that in the source code. Instead we just do what the typechecker would do when confronted with such a term. Search for `ASSOCIATIVE_OPS` or `CHAIN_PREDS` to see the code.