## Open questions
### Macros
What kind of macros do we support, and when are they expanded?
I tentatively want to:

- replace `(Macro ...)` and `(BlockMacro ...)` expressions with first-class, pure, nonrecursive functions.
- replace `(ifflag <boolflag> <any-sexpressions>)` macros with an *optional* partial evaluation -> simplification facility that NLG and formal methods can make use of.
- this means that typechecking does not get to assume that such "macros" have been expanded.

Benefits of this approach:

- Enables us to do type-driven, formal-methods-driven rewrites of human-written L4 source.
- Ensures that we can always link from an NLG doc back into source file, rather than into an intermediate file.
- But we will still have the benefit of being able to view simplified versions (with "preprocessor" macros fixed) of both the source code and the NLG documents.

### Simplifications phases before formal methods?

Pros:

- Simplifications phases (e.g. elimination of `must`) reduce redundancy in the formal methods code, which in some cases will already be very complex.
- Simplifications phases result in simplified mental models.

Cons:

- Simplification phases complicate giving accurate L4 source positions for errors.
- Simplification phases arguably complicate giving intelligible error messages. We should not have obscure error messages that need to be googled to understand.
- If we want exhaustive pattern matches, then we might need yet another version of the L4 contract model (in addition to AST, L-AST, TL-AST; see below).
    - Though not necessarily: dependent types are an alternative, for example.

Possible middle ground: JIT simplification.

- This is a commonly employed approach, which in many cases recovers the above "Pros" of simplification phases.


## Architecture

- AST (a collection of types in a module, say) defines the immutable AST.
    - This is the parser's target.
    - We do minimal error checking constructing the AST. For example, symbols are not resolved to their definitions.
    - I would like to resist doing even macro expansion of preprocessor macros.
    - Though not the main goal, one consequence of this is that document order should not matter for anything, e.g. it's not necessary to have a definition of a symbol precede the usage of the symbol, provided there is no actual cyclic dependency.

- L-AST (linked AST) is a more-convenient, more-strongly-typed model of the contract.
    - The "more-convenient" part makes for simpler program analysis/transformation code. The "more-strong-typed" part does too, by removing the need to handle (or " unsafely" not handle) impossible cases that the type system doesn't prevent.
    - For example, if `app` is a function symbol application, then `app.fn` is a feature-rich model of the function symbol, rather than just a `Symbol`.
      Thus, our implementation language will not ask us to handle the case where the symbol does not resolve to a FnSymbol object in our global map of `Symbol`s  to `FnSymbol` objects (which would be the alternative approach, if we don't use L-AST).
    - Syntax error checking happens here, but not typechecking yet. AST nodes with optional t ype annotations have a `.sort : Option[Sort]` field.
    - AST is still the source of truth, and that is somtimes what we transform when we do transformations. L-AST is then rebuilt.
    - Since we want to do pattern matching on L-AST types also, in Scala we would need to make them immutable case classes as well.
    - L-AST types might wrap AST types. E.g. if t : L-AST.Term, then t.ast : AST.Term.

- TL-AST (typed linked AST) types are identical to L-AST types, but every L-AST node with a `.sort : Option[Sort]` field has a `.sort : Sort` field.
    - This datastructure could be automatically generated from L-AST.
    - In principle, I would like for us to be able to output to an AST object that has explicit type annotations on everything. This simplifies our mental model, makes our formal methods independent of type inference and intersection function-types.


