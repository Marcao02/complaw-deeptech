MiniL4 is a subset of L4 intended to be used to investigate different technologies for the next, high-quality implementation of L4.
Its features are intended to be a minimal set required to bring out most of the difficulties of implementing L4.

In particular, the following features are removed:

- Parsing. The few miniL4 examples we'll have will be constructed from AST constructors directly.
- `DateTime`. Note that in full L4, no contract or event parameters of sort `DateTime` are allowed. This essentially allows eliminating `DateTime` by converting all `DateTime` literals to `TimeDelta` literals using the contract's declared `StateDateTime`.
- `FollowingSituation`
- `Breach_*` and `Breached_*`
- `must` in its current form, but *will* have the more general `must ... lest ...`
- `TDMap`, `Int` (the only atomic sorts are `Bool`, and `Real` and its subtypes)
- `min`,`max`,`/`,`floor`,`ceil`,`^`,`fraction-of-sum`
- `ifthenelse` function symbol (not to be confused with the `IfElse` `Statement`, which miniL4 does have), unless we decide we should include it for the sake of another example program transformation task (eliminating it).
- Code groups `Definitions`, `SortDefinitions`, `ContractParams`, `StateVars`, `Dynamics`. But will still have the singular forms such as `Definition`, `SortDefinition`, `StateVarDec`.
- `Action`. This is replaced with `InteralEvent` and `ExternalEvent`.
- Function-like macros (which will probably not be in the full L4, either).
- All nlg stuff.

*Will* still have:

- Local variables, since removing them is a basic example program transformation task.
- `Dimensioned` sort constructor (the only sort constructor).