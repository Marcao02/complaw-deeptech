We can do the following monster burger formalization and NLG using sugared FOL and NLG code that we already know how to write. 
Here's the version of monster burger we're formalizing.

* Clause 1: When the Challenger orders the Monster Burger, their obligation to pay $50 for it before they leaves the restaurant becomes active.
* Clause 2: Once the Monster Burger is served, the Challenger wins iff they eat it within 1 hour.
* Clause 3: If the Challenger wins, their obligation to pay from Clause 1 is canceled.


The only difference between the desired NL and the generated NL (see below) is the difference between "their obligation to pay from Clause 1" and "the obligation from Clause 1". I personally prefer to include redundancy like "their" and "to pay" in the first form, because I think it makes sentences easier to read, but probably some of you will prefer the more concise form anyway.

I haven't explained everything in here yet. Here are some hurried explanations: 

* `the S from ‹reference›` resolves to the unique maximal term of sort S in ‹reference›, and is a compilation error if there is no such term or more than one such term.
* `pronoun`s work similarly, except that they resolve to something in the current or previous formula. 
* The statement `they's ↦ their` is only used by our NLG code. It tells us to replace "they's" with "their". Of course, that is something that we'd import from our reusable English module. 
* `ObligationWithDeadline ⊆ Obligation` is the most fundamental FOL version of subtyping (trivia: with it, you call this *order-sorted first order logic*). It means that any interpretation must make `ObligationWithDeadline` and subset of `Obligation`.


```
(Sorts 
	Person
	FoodOffer
	DollarAmount
	Event
	UnappliedObligation
	Obligation
	ObligationWithDeadline ⊆ Obligation
)

(ReservedVariables
	E,E1,E2,E3 : Event
	UO : UnappliedObligation	
	O : Obligation
	P : Person
	F : FoodOffer
	A : DollarAmount
)

pronoun they : Person
pronoun it : FoodOffer
they's ↦ their

(the Challenger) : Person
(the Monster Burger) : FoodOffer
(P orders F) : Person → Event
(P leaves the restaurant) : Person → Event
(pay A for F) : DollarAmount × FoodOffer → UnappliedObligation
```

An `UnappliedObligation` is essentially a qualitative description of an obligation. The next function symbol takes such a description and attaches it to a person and a deadline event, to get a normal obligation, which we currently call `ObligationWithDeadline`.

```
(P's obligation to UO before E) : Person × UnappliedObligation × Event → ObligationWithDeadline
```

This means the obligation O is inactive before E and active for some period of time after E. If and when it is later inactive again is undetermined.

```
(When E, O becomes active) : Event × Obligation → Bool
```
‹Logic note› O is an immutable object, like everything in FOL, so it doesn't actually change from the formal logic perspective.
 
‹Logic note› See https://docs.google.com/document/d/1oLJ_cGlvJgKjQniQSgcJTnSZsZG2wP6CaKcCCyWsvsQ/edit# for question of what syntax to use to indicate the type of a predicate symbol. I waiver between this and a few alternatives. 

This is the concrete syntax for defining Clause 1:

```
(Clause 1) := (When ((the Challenger) orders (the Monster Burger)), 
     (they's obligation to (pay $50 for it) before (they leaves the restaurant)) becomes active.)
```

Our GF code will fix the agreement to generate exactly the sentence we wanted:

Clause 1: When the Challenger orders the Monster Burger, their obligation to pay $50 for it before they leaves the restaurant becomes active.

```
(F is served) : FoodOffer → Event
(P wins) : Person → Event
(P finishes F) : Person × Food → Event
(one hour) : Timespan
```

This means that after E1 happens, the following relation holds: (E2 happens) if and only if (E3 happens within timespan T)
```
(Once E1, E2 iff E3 within T) : Event × Event × Event × Timespan → Bool
```

This means that *if* E happens, the obligation O is inactive from the time of that event onward.
```
(If E, O is canceled) : Event × Obligation → Bool
```

```
(Clause 2) := (Once ((the Monster Burger) is served), ((the Challenger) wins) iff (they finishes it) within (one hour))
```

We generate exactly what we wanted (again using GF code to fix agreement):

(Clause 2): Once the Monster Burger is served, the Challenger wins iff they finish it within one hour.

```
(Clause 3) := (If ((the Challenger) wins), (the Obligation from (Clause 1)) is canceled)
```

We generate, not *exactly* the sentence we wanted, but quite close:

Clause 3: If the Challenger wins, the obligation from Clause 1 is canceled.

@chiahli @gauntlet173 @mengwong @alexis 






<!-- ; `When E1, P acquires the obligation to UO before E2` : Event × Person × UnappliedObligation × Event × Obligation → * 
`When E1, E2` : Event × Event → *
`P acquires the obligation O to UO before E2` : Event × Person × Obligation × UnappliedObligation × Event → Event
`When E1, P acquires the obligation to UO before E2` : Event × Person × UnappliedObligation × Event → Obligation  
Happens((the Challenger) orders the Monster Burger) → O becomes active
-->

<!--Clause1 := ∃!o:Obligation. (When ((the Challenger) orders the Monster Burger), (the Challenger) acquires the obligation to (pay $50) before ((the Challenger) leaves the restaurant) o)
-->
<!--
----------------------------------------


Do we want to say that the restaurant's canceling of the payment obligation is itself an obligation? If so, it's a kind of obligation that, depending on if the customer orders anything else and how they pay, if we model it accurately enough, can only be violated, not (permanently) fufilled, since at any point in the future the restaurant could violate the obligation by charging the customer's credit card (if they have the customer's credit card number).
(1) The restaurant doesn't insist on payment before the customer leaves.
(2) The restaurant _never_ charges the customer's credit card for the monster burger.



----------------------------------------

Sorts 

* `Actor`
* `Time`, `Duration`
* `Task`
* `Obligation`, `ObligationWithDeadline`
* `Contract`

```
‹monster burger contract› : Contract
‹the start of this challenge› : Time
```

> (1) The Challenger must pay $50 to Bob’s Burgers.

‹the Challenger must pay $50 to Bob’s Burgers› : Obligation

> "This is due in 3601 seconds." 

We rewrite that to: ```This must be done within 3601 seconds of the start of this challenge.```

```
❬___ is satisfied within ___ of ___❭ : Obligation × Duration × Time → Obligation
‹3601 seconds› : Duration
```

```
‹1› : Obligation := let ‹this› := ‹the Challenger must pay $50 to Bob’s Burgers›
		in ❬‹this› is satisfied within ‹3601 seconds› of ‹the start of this challenge›❭
```

> (2) The Challenger must complete the Food Challenge, consisting of:
> > (2a) eating the whole 1kg Monster Burger and Fries; and
> > 
> > (2b) complete the obligation in clause (2a), above, within 1 hour (3600 seconds) of the start of this challenge.

```
‹2a› : Obligation := ‹the challenger must eat the whole 1kg Monster Burger and Fries› 
‹3600 seconds› : Duration
‹2b› : Obligation := ❬‹2a› is satisfied within ‹3600 seconds› of ‹the start of this challenge›❭
both : Obligation × Obligation → Obligation
‹2› := both(‹2a›,‹2b›)
```

> (3) Immediately upon the satisfaction of the above, the Restaurant must waive any cost of the 1kg Monster Burger and Fries in clause (2a) above.

```
❬___ is satisfied by ___› : Obligation × Time → *
❬immediately after satisfying _____, must _____❭ : Obligation × Obligation → Obligation
‹waive cost› : Obligation
‹3› : Obligation := ❬immediately after satisfying ‹2›, must ‹waive cost›❭
```

‹waive cost› means ‹a› gets canceled.
```
∀t:Time. ∀d:Duration. ❬‹waive cost› is satisfied within d of t❭
‹the Restaurant must waive any cost of the 1kg Monster Burger and Fries› : Obligation := ‹waive cost›(‹2a)

```

------------------------


‹complete ___ within ___ of ___ : Task × Duration × Time → Task
‹2b› := ‹complete ‹2a› within ‹3600 seconds› of ‹the start of this challenge››
‹the food challenge› : Task := and(‹2a›, ‹2b›)
‹The Challenger MAY complete ____› : Task → Obligation

‹2› := ‹The Challenger MAY complete ‹the food challenge›› : Obligation

‹3› := "The restaurant must wave any cost of the Monster Burger and Fries"


‹challenger owes $30 for a monster burger and fries› : Obligation


‹challenger eats the entire monster burger and fries› : Task 
‹___ within ___ of the start of the challenge› : Task × Duration → *
‹one hour› : Duration


-->