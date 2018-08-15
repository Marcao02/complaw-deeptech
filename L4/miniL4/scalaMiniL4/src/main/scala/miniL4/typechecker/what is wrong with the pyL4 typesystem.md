- The std lib fn type declaration list is very large, and could be much smaller (lots of boilerplate).
    - Bad for typechecking error messages, for example.
- Std lib fn types are not exhaustive, so it's occasionally necessary to add to them in pyL4.
- Support for (only!) two dimensioned units (one integer, one real) is basically hardcoded. Adding more wouldn't be much trouble, but that's even more boilerplate.
- The allowed datatypes involving sort ops are adhoc.
- No plan in place for user-introduced datatypes (aside from dimensioned numerics and datatype definitions).
- Symmetric functions are not handled well.

## Ideas for improvement:

- Introduce explicit judgement for datatype validity/nonemptyness.
- Put more derivations as runtime inference rules rather than part of the exhaustive graph.
	- try to do this for dimensioned datatypes
	- try to do this for Ratio datatypes
- Formalize explicit datatype sets, as in UnboundedNumericSorts in pyL4

## Current candidate (8 Aug 2018)

- All datatypes that need to be used during typechecking must be declared in the contract.
    - But have a big set of useful defaults always added, and for it can pregen the subtyping graph.
- The declared datatypes are used to instantiate the parametric subtyping decs.
- Fn types are not instantiated until typechecking time.

## Candidate pipeline

1. Eliminate symmetric fn type declarations
2. (Virtually) eliminate arb arity function apps
3. Then "Current candidate" based type checking


### Example: ideal fn type def for multiplication

NOTE: {} represents symmetry, i.e. {s,t} basically represents s × t and t × s

MULT\_CLOSED\_NUMERICS =  
{'{0}','{1}','{0,1}','(0,1]','[0,1]','[0,1)','(0,1)',PosReal,NonnegReal,Real}  
∀s ∈ MULT\_CLOSED\_NUMERICS  
__ {s, s} ⟶ s  

; next two for scaling  
∀u ∈ contractDims, ∀s ∈ MULT\_CLOSED\_NUMERICS  
__ {dim(s,u), s} ⟶ dim(s,u)  

UNBOUNDED\_NUMERICS = {PosReal,NonnegReal,Real}  
∀u ∈ contractDims, ∀s ∈ UNBOUNDED\_NUMERICS,  
__ {dim(s,u), PosReal} ⟶ dim(s,u)  

---------
Remark: Ideally, the above would hold for certain Ratio types. For example:  
{dim(s,u)/dim(PosReal,v), PosReal} ⟶ dim(s,u)/dim(PosReal,v)  
One possible mechanism for that is to introduce SCALABLE datatype set. Example:  
∀u ∈ contractDims, ∀s ∈ Scalable,  
__ {dim(s,u), PosReal} ⟶ dim(s,u)  
And elsewhere we have rules:  
__ UnboundedNumeric(s) ⟶ Scalable(s)  
__ Scalable(s) ⟶ Scalable(dim(s,u)/dim(PosReal,v})

---------

### Example using rewrite systems (see "typesystem rewrite pseudocode.txt" in pyL4)

u,v are the units match vars.  
a,b,c are the datatype match vars.  

; this does the trick without creating reals of things that only make sense as integers:  
(datatype Real), etc  
(datatype (dim s u)) & (subtype s' s) => (datatype (dim s' u))  
; oh but prevous rule is very nondeterministic. this is better:    
(datatype,dim(s,u)) & (datatype,dim(s',u)) & s ⊆ s' => dim(s',u) ⊆ dim(s,u)  

The judgements:

- ('subtype,\<sort>,\<sort>)
- ('datatype,\<sort>)
- ('sft,\<fnsymb>,\<sort>,...,\<sort>)  i.e. simple function type
- ('union',\<sort>,\<sort>,\<sort>)
