
We can do the following monster burger formalization and NLG using sugared FOL and NLG code that we already know how to write. 
Here's the version of monster burger we're formalizing.

* Clause 1: When the Challenger orders the Monster Burger, their obligation to pay $50 for it before they leaves the restaurant becomes active.
* Clause 2: Once the Monster Burger is served, the Challenger wins iff they eat it within 1 hour.
* Clause 3: If the Challenger wins, their obligation to pay from Clause 1 is canceled.


The only difference between that desired NL and the (manually) generated NL given below is in part of Clause 3: 

* Desired:   "their obligation to pay from Clause 1"
* Generated: "the obligation from Clause 1" 

I personally prefer to include redundancy like "their" and "to pay" as in the first form, because I think it makes sentences easier to read. I imagine some people will prefer the more concise second form. Let me know if that's the case. 

I haven't explained all the syntactic sugar used in here yet. Here are some hurried explanations: 

* `the S from ‹reference›` resolves to the unique maximal term of type S in ‹reference›, and is a compilation error if there is no such term or more than one such term.
* `pronoun`s work similarly, except that they resolve to something in the current or previous formula. 
* The statement `they's ↦ their` is only used by our NLG code. It tells us to replace "they's" with "their". Of course, that is something that we'd import from our reusable English module. 
* `ObligationWithDeadline ⊆ Obligation` is the most fundamental FOL version of subtyping (trivia: with it, you call this *order-sorted first order logic*). It means that any interpretation must make `ObligationWithDeadline` and subset of `Obligation`.

## Minimal formalization for NLG

This is enough to generate good English. More axioms are required to prove anything interesting. I'll go into that in the next section.

```
(Types 
	Person
	FoodOffer
	DollarAmount
	Event
	Obligation
	ObligationWithDeadline ⊆ Obligation
	ActionRelation
)

(ReservedVariables
	E,E1,E2,E3 : Event
	O : Obligation
	P : Person
	F : FoodOffer
	A : ActionRelation
	M : DollarAmount
)

pronoun they : Person
pronoun it : FoodOffer
they's ↦ their

(the Challenger) : Person
(the Monster Burger) : FoodOffer
(P orders F) : Person → Event
(P leaves the restaurant) : Person → Event
(pay M for F) : DollarAmount → FoodOffer → ActionRelation
```

<!--An `ActionRelation` is a set of actions. -->

The next function symbol takes an ActionRelation and attaches it to a Person and a deadline Event, to get a normal obligation, which we currently call `ObligationWithDeadline`.

```
(P's obligation to A before E) : Person → ActionRelation → Event → ObligationWithDeadline
```

This means the obligation O is inactive before E and active for some period of time after E. If and when it is later inactive again is undetermined.

```
(When E, O becomes active) : Event → Obligation → Bool
```

*‹Logic note›* O is an immutable object, like everything in FOL, so it doesn't actually change from the formal logic perspective.
 
*‹Logic note›* See [True Names](https://docs.google.com/document/d/1oLJ_cGlvJgKjQniQSgcJTnSZsZG2wP6CaKcCCyWsvsQ/edit#) for question of what syntax to use to indicate the type of a predicate symbol. I waiver between `→ Bool` and a few alternatives. Probably we will end up taking the Simple Theory of Types route.

This is the concrete syntax for defining Clause 1:

```
(Clause 1) := (When ((the Challenger) orders (the Monster Burger)), 
     (they's obligation to (pay $50 for it) before (they leaves the restaurant)) becomes active.)
```

Our GF code will fix the agreement to generate exactly the sentence we wanted:

Clause 1: When the Challenger orders the Monster Burger, their obligation to pay 50 dollars for it before they leave the restaurant becomes active.

```
(F is served) : FoodOffer → Event
(P wins) : Person → Event
(P finishes F) : Person → Food → Event
(one hour) : Timespan
```

Next predicate symbol means that after E1 happens, the following relation holds: (E2 happens) if and only if (E3 happens within timespan T).

```
(Once E1, E2 iff E3 within T) : Event → Event → Event → Timespan → Bool
```

Next predicate symbol means that *if* E happens, then the obligation O is inactive from the time of that event onward.

```
(If E, O is canceled) : Event → Obligation → Bool
```

```
(Clause 2) := (Once ((the Monster Burger) is served), ((the Challenger) wins) iff (they finishes it) within (one hour))
```

We generate exactly what we wanted (again using GF code to fix agreement):

_Clause 2_ : Once the Monster Burger is served, the Challenger wins iff they finish it within one hour.

```
(Clause 3) := (If ((the Challenger) wins), (the Obligation from (Clause 1)) is canceled)
```

We generate, not *exactly* the sentence we wanted, but quite close:

_Clause 3_: If the Challenger wins, the obligation from Clause 1 is canceled.


## Constraining axioms
We need to settle on some notation to refer to function symbols that use mixed-fix notation. Options:

* `(·'s obligation to · before ·)` or `(_'s obligation to _ before _)`
* `λP,A,E. (P's obligation to A before E)`. 

Write in [True Names](https://docs.google.com/document/d/1oLJ_cGlvJgKjQniQSgcJTnSZsZG2wP6CaKcCCyWsvsQ/edit#) if you have a preference.


As usual, unbound variables are implicitly universally quantified.

The following axioms will generate comprehensible English sentence, but they do not need to generate idiomatic sentences. They are only used to clarify our prose definitions of the symbols that we use directly in the contract.

So far we haven't formally specified significant meaning to obligations. We'll do that now.

An Obligation O has at least the following not-all-independent things:

* An `Entity`, `(the Entity obligated by O)`. Note for this example we use its subset-type `Person`.
* An `ActionRelation`, `(actions fulfilling O)`. This is the criteria for fulfilling the obligation.
* A TimeSpan when the obligation is *active*.
* A truth value, `(O is met)`, which is determined by whether `e = (the Entity obligated by O)` does an action `a` during `(O's period of activity)` 


---------------

```
(E is O's deadline),  : Event → Obligation → Bool
(O is inactive before E), (O is active at E) : Obligation → Event → Bool
(O is met) : Obligation → Bool
(P does A after E), (P does A before E) : Person → ActionRelation → Event → Bool
```

The first axiom is definitional in nature:

```
(Axiom 1) := O = (P's obligation to A before E) ⇒ (E is O's deadline)
```

Meeting the obligation requires doing the obligated action before the deadline:

```
(Axiom 2) := O = (P's obligation to A before E) ∧ (O is met) ⇒ (P does A before E)
```


```
(Axiom 3) := (When E, O becomes active) ⇒ (O is inactive before E) ∧ (O is active at E)
```


A strengthening of the previous axiom's conclusion applies when we also know when the obligation became active:

```
(Axiom 3) := (When E1, O becomes active) ∧ O = (P's obligation to A before E2)  
          ⇒ ((O is met) ⇔ ((P does A after E1) ∧ (P does A before E2))
```





**Note**: We haven't yet expressed the constraint that A must happen after the obligation becomes active in order for the obligation to be met.