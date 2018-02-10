Note that MatchVar(·) is not yet formalized, so it might be used inconsistently. 

I have not worked out how exactly I should implement the MatchVars part.

**Idea**: Constraint with MatchVar's don't get added to the graph (call them "parametric constraints"). Only their concrete instances do.

For first implementation, whenever a new concrete sort is encountered, it is matched against the left and right sides of every parametric constraint. 
But wait... that should only be done for constraints up to the given complexity.

# Subtyping

Later: language for saying that a sort is empty, e.g. NonnegReal[shares].

The _complexity_ of a sort is its size, defined in the usual way, starting from 1. E.g. TDMap[Tuple[Nat,Nat]] has complexity 4.

The _complexity_ of a subtyping relation S₁ ≤ S₂ is the maximum of the complexities of S₁ and S₂.

All sub relns between complexity 1 sorts are always added.

## Dimensioned numeric sorts

These are complexity 2.
`u,v: MatchVar(DimUnit)`

`PosInt[u] ⊆ Nat[u]`, # makes sense for counting any kind of thing
`PosReal[u] ⊆ NonnegReal[u]` # makes sense for measuring any kind of thing

`NonnegReal[u] ⊆ Real[u]` # unfortunately seems to currently be needed for subtraction to be total
`Nat[u] ⊆ Int[u]` # unfortunately seems to currently be needed for subtraction to be total

## Ratios of nondimensioned numeric sorts

These are complexity 3.

`Ratio(NonnegReal, PosReal) ⊆ NonnegReal`
`Ratio(NonnegReal, PosInt) ⊆ NonnegReal`
`Ratio(PosReal, PosReal) ⊆ PosReal`
`Ratio(PosReal, PosInt) ⊆ PosReal`
`Ratio(Real, PosReal) ⊆ Real`
`Ratio(Real, PosInt) ⊆ Real`

```
for num1 in UnboundedNumericSorts:
    for num2 in UnboundedNumericSorts:
        if not graph.hasEdge(num1,num2):
            continue
        for den1 in [PosReal,PosInt,"{1}","(0,1)","(0,1]"]:
            for den2 in [PosReal,PosInt,"{1}","(0,1)","(0,1]"]:
                if not graph.hasEdge(den1,den2):
                    continue
                graph.addEdge(Ratio(num1,den1), Ratio(num2,den2))
```


## Ratios of dimensioned numeric sorts

These are complexity 4. Eventually these should be derived from `Ratio` section above, but fine to hardcode for now.
`u,v: MatchVar(DimUnit)`

`Ratio(PosReal[u], PosInt[v]) ⊆ Ratio(NonnegReal[u], PosInt[v]) ≤ Ratio(Real[u], PosInt[v])`
`Ratio(NonnegReal[u], PosReal[u]) ⊆ Ratio(PosReal[u], PosReal[u]) ≤ Ratio(Real[u], PosReal[u])`
`Ratio(PosReal[u], PosReal[u]) ⊆ PosReal`

## Tuple

These have complexity 3 or greater (unbounded).

`Sᵢ,Tᵢ: MatchVar(Sort)`

Complexity max(c₁ + ... + cₖ, d₁ + ... + dₖ) + 1 constraints derived from complexity cᵢ sorts Sᵢ, complexity dᵢ sorts Tᵢ:

`S₁ ⊆ T₁,...,Sₖ ⊆ Tₖ  implies  Tuple(S₁,...,Sₖ) ⊆ Tuple(T₁,...,Tₖ)`

## TDMap

A map from some sort S to TimeDelta. We simply haven't used maps for other value types yet.

Not currently using subtyping with it either.

`S: MatchVar(Sort)`

From complexity k sort S, generate complexity k+1 constraint:
`TDMapEmpty ⊆ TDMap[S]`


------------

# Function types

The complexity of a simple function type S₁ → ... → Sₖ is the maximum of the complexities of the Sᵢ.

Complexity 1 function types are always included. Those include a lot of numeric function types. But also some types involving `Bool`, `TimeDelta`, `emptyTDMap`, `DateTime`, `RoleId`,

## TDMap

`X: MatchVar('Sort')`

Complexity k+1 constraints derived from complexity k sort X:

```
(('mapSet',), parametric_one_var( (
        sfntype('EmptyTDMap', X, 'TimeDelta', SApp('TDMap', X)),
        sfntype(SApp('TDMap', X), X, 'TimeDelta', SApp('TDMap', X))
        ),
        TDMapKeySorts)
     ),
(('tdGEQ',), parametric_one_var(
    sfntype(SApp('TDMap', X), X, 'TimeDelta', 'Bool'),
    TDMapKeySorts)
 ),
(('mapDelete',), parametric_one_var(
    sfntype(SApp('TDMap', X), X, SApp('TDMap', X)),
    TDMapKeySorts)
 ),
(('mapHas',), parametric_one_var(
    sfntype(SApp('TDMap', X), X, 'Bool'),
    TDMapKeySorts)
 ),
(('nonempty','empty'), parametric_one_var(
    sfntype(SApp('TDMap', X), 'Bool'),
    TDMapKeySorts))
```

## Numeric functions

See `standard_function_types.py`. I think I've got non-dependent types figured out now.

----------------------------
# Dependent function types

Currently have only two: (Sing a) for the singleton {a}, and (LEQ x) for the numbers (or numbers of something, in the case of Real[u], Int[u]) less than or equal to x.

Also these require introducing the sort operator & for ⋂, since we now have to accept terms whose simplest type has the form 
`‹non-dependent sort› & ‹dependent sort›`

or even

`‹non-dependent sort› & ‹dependent sort 1› & ...`
` & ‹dependent sort k›`

The _complexity_ of a term is its size, defined in the usual way starting from 1. The _complexity_ of a dependent sort of the form `(R t)` is 1 + complexity(t).

Typechecking with dependent types requires a context Γ for the types of free variables, which are action parameters or local variables. Here's an example true relation (or provable "judgement", as the pretentious type theory people like to say), where `PURCHASE_AMOUNT` is a contract parameter of type `PosReal[$] & (Sing PURCHASE_AMOUNT)`.

```
purchase_amount_reduction : NonnegReal[$] & LEQ[PURCHASE_AMOUNT] ⊢ 
		PURCHASE_AMOUNT - purchase_amount_reduction) : NonnegReal[$]
```

The simple function type of `-` used to typecheck that example is

```
u: MatchVar(DimUnit)
x: MatchVar(NonnegReal[u])
(Sing x) -> (Real[u] & (LEQ x)) -> NonnegReal[u]
```
This is also true:
```
(Sing x) -> (NonnegReal[u] & (LEQ x)) -> (NonnegReal[u] & (LEQ x))
```

In both cases, the complexity of the function type is complexity(term matched to x) + 4. 

If we drop the dimensioned numeric units, then you can read the second function type above, when attached to `-`, as the following obvious fact:

For every x,y ∈ ℝ, if 0 ≤ x and 0 ≤ y ≤ x then 0 ≤ x - y ≤ x.

Which is true more generally: 

For every x₁,x₂,y ∈ ℝ, if 0 ≤ x₂ ≤ x₁ and x₂ ≤ y ≤ x₁ then x₂ ≤ x₁ - y ≤ x₁.

We could, conversely, with a more-general 2-argument dependent sort operator (BTW a b) (between a and b), write the more general form as the function type

```
x₁,x₂,y : MatchVar(NonnegReal)
(Sing x₁) -> ((Sing y) & (BTW x₂ x₁)) -> (BTW x₂ x₁)
```

We would also, in that case, have information such as 

`(BTW a b) ⊆ (LEQ b)`