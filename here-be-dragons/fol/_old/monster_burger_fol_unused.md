
; `When E1, P acquires the obligation to UO before E2` : Event × Person × UnappliedObligation × Event × Obligation → * 
`When E1, E2` : Event × Event → *
`P acquires the obligation O to UO before E2` : Event × Person × Obligation × UnappliedObligation × Event → Event
`When E1, P acquires the obligation to UO before E2` : Event × Person × UnappliedObligation × Event → Obligation  
Happens((the Challenger) orders the Monster Burger) → O becomes active


Clause1 := ∃!o:Obligation. (When ((the Challenger) orders the Monster Burger), (the Challenger) acquires the obligation to (pay $50) before ((the Challenger) leaves the restaurant) o)

----------------------------------------


Do we want to say that the restaurant's canceling of the payment obligation is itself an obligation? If so, it's a kind of obligation that, depending on if the customer orders anything else and how they pay, if we model it accurately enough, can only be violated, not (permanently) fufilled, since at any point in the future the restaurant could violate the obligation by charging the customer's credit card (if they have the customer's credit card number).
(1) The restaurant doesn't insist on payment before the customer leaves.
(2) The restaurant _never_ charges the customer's credit card for the monster burger.



----------------------------------------

Sorts 

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


