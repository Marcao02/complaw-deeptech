# **INCOMPLETE**
copied from now-closed issue https://github.com/legalese/legalese-compiler/issues/36

# Idea
This reduction works for `must-later`/`may-later` declarations that have no time constraint or a time constraint of the form event\_ts ≤ E for some expression E.

For each role R and action A that takes parameters of type T, such that R has a must-later A-rule, a state variable (aka global variable) is introduced:

`must_R_A : (Map T TimeDelta)`

Similarly if R has a may-later A-rule, the state variable

`may_R_A : (Map T TimeDelta)`

is introduced.

`(Map K V)` is a parametric immutable (all L4 datatypes are immutable) datatype with functions:

`mapSet : (Map K V) -> K -> V -> (Map K V)`
`mapDelete : (Map K V) -> K -> (Map K V)`
`mapHas : (Map K V) -> K -> Bool`
`mapDateTimeGT : (Map K DateTime) -> K -> DateTime -> Bool`

The last requires an explanation: `(mapDateTimeLEQ m k dt )` is false if `k` is not in `m`, and if `k` is in `m` then it's true iff `m[k] > dt`.

See Jan 4 discussion on Slack dsl channel.

# Translation

## In action declarations

If a rule of the following form occurs in the `Future` section of an `Action` declaration:
```
(if bool_term
	(R must-later [A arg₁ arg₂] [event_td ≤ timedelta_term])
)
```
it is removed and the following is appended to the `Transform` section of the same `Action` declaration:
```
(if bool_term
	(must_R_A = [mapSet must_R_A (tuple arg₁ arg₂) timedelta_term])
)
```
`may-later` is handled identically exception that `may_R_A` is used instead.

## In state declarations

Suppose a `State` (aka `Section`) declaration contains

`[possibly-from-earlier R must A]`

where `A` is an action with (say) 2 parameters. Then it is removed and the following `NextActionRule` gets added to the same `State`:

```
(R obligation-options-include
	(A ?1 ?2)
	(tdGT must_R_A (tuple ?1 ?2) event_td)	
)
```
The line `(tgGT ...)` compares (i) the deadline that `must_R_A` has stored for `(tuple ?1 ?2)`, and (ii) the current event's timedelta.
We don't need a where clause such as `(where (mapHas must_R_A (tuple ?1 ?2)))` because it is implied by the time constraint.

`[possibly-from-earlier R may A]` gets handled similarly except that `may` is used instead of `obligation-options-include` in the translation.