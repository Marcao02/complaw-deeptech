# MB with restaurant obligations to serve and check finished on time.

## Prose

We can do the following monster burger formalization and NLG using sugared FOL and NLG code that we already know how to write. 
Here's the version of monster burger we're formalizing.

<!--* Clause A1: A monster burger (MB) challenge is between the Challenger and the Restaurant, and a customer can only enter one MB challenge per visit to the Restaurant.-->
* Clause A1: Once the Challenger orders the Monster Burger, the Restaurant's obligation to serve the Monster Burger to the Challenger within 15 minutes becomes active.
* Clause A2: Once the Restaurant fulfills the obligation of Clause A1, the Challenger's obligation to pay $50 for it before they leave the restaurant becomes active. 
* Clause A3: If the obligation of Clause 1 is failed, then the contract is fulfilled. 
* Clause 2: Once the restaurant serves the Monster Buger to the Challenger, the Challenger wins iff they eat it within 1 hour.
* Clause 3: If the Challenger wins, their obligation to pay from Clause 1 is canceled.


* Clause A2: Once the Challenger orders the Monster Burger, their obligation to pay $50 for it before they leave the restaurant becomes active.
* Clause A3: If the Restaurant fails to meet the deadline of the obligation of Caluse A1, then the price of Monster Burger is reduced to $40. 
* Once the Challenger orders the Monster Burger, the Restaurant's obligation to serve the Monster Burger to the Challenger within 15 minutes becomes active.
* Clause 1: Upon the Challenger ordering the Monster Burger, their obligation to pay $50 for it before they leave the restaurant becomes active.
* Clause A3: If the Challenger finishes the MB within {TIME_TO_EAT_BURGER} of being served, then the Challenger wins. 
* Clause A4: When the Challenger orders a MB, the restaurant guarantees they will serve the burger within {MAX_SERVE_DELAY}. Otherwise, the customer is not obligated to pay for the burger (but the restaurant may still serve the burger late).
* Clause A5: The customer can claim to be finished early, upon which the restaurant must within ({MAX_CLAIM_VERIFICATION_DELAY}, or by the end of the {TIME_TO_EAT_BURGER} challenge period -- whichever comes first) confirm or reject the claim. If no such "early finish" claim is made, the restaurant checks after {TIME_TO_EAT_BURGER} from serving time.
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
	MenuItem
	DollarAmount
	Event
	Obligation
	ObligationWithDeadline ⊆ Obligation
	ActionType
)

(ReservedVariables
	E,E1,E2,E3 : Event
	O : Obligation
	P : Person
	F : MenuItem
	A : ActionType
	M : DollarAmount
)

pronoun they : Person
pronoun it : MenuItem
they's ↦ their

(the Challenger) : Person
(the Monster Burger) : MenuItem
(P ordering F) : Person → MenuItem → Event
(P leaves the restaurant) : Person → Event
(pay M for F) : DollarAmount → MenuItem → ActionType
```

<!--An `ActionType` is a set of actions. -->

The next function symbol takes an ActionType and attaches it to a Person and a deadline Event, to get a normal obligation, which we currently call `ObligationWithDeadline`.

```
(P's obligation to A before E) : Person → ActionType → Event → ObligationWithDeadline
```

If E occurs, the following means the obligation O is inactive before E, and active immediately following E. If and when it is later inactive once again is undetermined. If E does not occur, the relation is defined to be true.

```
(Upon E, O becomes active) : Event → Obligation → Bool
```

**Compare that** to the related (currently unused) symbol of the same type, which says only that an obligation is active immediately after an event, without also saying that it was inactive before: `(O is active immediately after E)`

*‹Logic note›* O is an immutable object, like everything in FOL, so it doesn't actually change from the formal logic perspective.
 
*‹Logic note›* See [True Names](https://docs.google.com/document/d/1oLJ_cGlvJgKjQniQSgcJTnSZsZG2wP6CaKcCCyWsvsQ/edit#) for question of what syntax to use to indicate the type of a predicate symbol. I waiver between `→ Bool` and a few alternatives. Probably we will end up taking the Simple Theory of Types route.

This is the concrete syntax for defining Clause 1:

```
(Clause 1) := (Upon ((the Challenger) ordering (the Monster Burger)), 
     (they's obligation to (pay $50 for it) before (they leaves the restaurant)) becomes active)
```

Our GF code will fix the agreement to generate exactly the sentence we wanted:

**__Clause 1__**: Upon the Challenger ordering the Monster Burger, their obligation to pay 50 dollars for it before they leave the restaurant becomes active.

```
(F is served) : MenuItem → Event
(P wins) : Person → Event
(P finishes F) : Person → Food → Event
(one hour) : Timespan
```

Next predicate symbol means that after E1 happens, the following relation holds: (E2 happens) if and only if (E3 happens within timespan T). This essentially defines E2 in terms of E1, E3 and T.

```
(Once E1, E2 iff E3 within T) : Event → Event → Event → Timespan → Bool
```

You see that, and you might want to break it up more. For example, you might want to make `Once` a symbol that takes an `Event` and a formula and yields a formula. That would be essentially the modal logic approach. That would feel more right, more satisfying, but it wouldn't actually benefit us.



Next predicate symbol means that *if* E happens, then the obligation O is inactive from the time of that event onward.

```
(If E, O is canceled) : Event → Obligation → Bool
```

```
(Clause 2) := (Once ((the Monster Burger) is served), ((the Challenger) wins) iff (they finishes it) within (one hour))
```

We generate exactly what we wanted (again using GF code to fix agreement):

**_Clause 2_** : Once the Monster Burger is served, the Challenger wins iff they finish it within one hour.

```
**(Clause 3)** := (If ((the Challenger) wins), (the Obligation from (Clause 1)) is canceled)
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
* An `ActionType`, `(actions fulfilling O)`. This is the criteria for fulfilling the obligation.
* A TimeSpan when the obligation is *active*.
* A truth value, `(O is met)`, which is determined by whether `e = (the Entity obligated by O)` does an action `a` during `(O's period of activity)` 


---------------

```
(E is O's deadline),  : Event → Obligation → Bool
(O is inactive before E), (O is active at E) : Obligation → Event → Bool
(O is met) : Obligation → Bool
(P does A after E), (P does A before E) : Person → ActionType → Event → Bool
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
(Axiom 3) := (Upon E, O becomes active) ⇒ (O is inactive before E) ∧ (O is active at E)
```


A strengthening of the previous axiom's conclusion applies when we also know when the obligation became active:

```
(Axiom 3') := (Upon E1, O becomes active) ∧ O = (P's obligation to A before E2)  
          ⇒ ((O is met) ⇔ ((P does A after E1) ∧ (P does A before E2))
```

**Note** (actually, is this still true?): We haven't yet expressed the constraint that A must happen after the obligation becomes active in order for the obligation to be met.


<!--
## English version 2

Upon : 

* Clause 1: After the Challenger orders the Moster Burger, they are obligated to pay $50 for it before they leave the restaurant. 
* Clause 2: Once the Monster Burger is served, the Challenger wins iff they eat it within 1 hour.
* Clause 3: If the Challenger wins, their obligation to pay from Clause 1 is canceled.

(After E1, P is obligated to A before E2) : Event → Person → Action → Event → 
-->