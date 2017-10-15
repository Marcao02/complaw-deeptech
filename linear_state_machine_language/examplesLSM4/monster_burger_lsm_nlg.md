<TransitionStatement> := ⟦Upon|Whenever⟧ <Event> <Statement>+
<Event> := <AtomicEvent> | (<Role> having just <Action>)
<ConditionalImmediateTransition> := (Event) | (if <Expression> <ImmediateTransition>)
<DeonticIntro> := (<Role> may next <Action> ⟦before|at or before⟧ <Event>) |
				  (<Role> may next <Action> [within <Duration>]) |
				  (<Role> may later <Action> within <Duration>) |
				  (<Role> must later <Action> within <Duration>)
<ConditionalDeonticIntro> := <DeonticIntro> | (if <Expression> <ConditionalDeonticIntro>)
<Statement> := <ConditionalDeonticIntro>
			 | (verifiedDisjointExhaustive <ConditionalDeonticIntro>+)
			 | (verifiedDisjoint <ConditionalDeonticIntro>+)
			 | (<Number> ≤ traversals ≤ ⟦<Number>|nonconstant⟧)
			 | (<Number> ≤ traversals)
			 | (traversals ≤ <Number>)
			 | (AllowedSubjects <Role>+)
			 | <ConditionalImmediateTransition>



* Upon `the Customer` `ordering the monster burger`:
	* `the Restaurant` should `serve the monster burger` within 15 minutes, and
	* otherwise `the Restaurant` `violates its prompt serve gaurantee`.

`serving the monster burger` is defined by `serve the monster burger` by a fixed grammatical relationship. The definition is  hidden by default, but revealed upon hovering or clicking on `serving the monster burger`. Let's call this an "unobtrusive  definition".

* Upon `serving the monster burger`:
	* `challenge endlimit time` is defined as the current time plus 1 hour, and
	* `amount owing` is set to $50, and
	* then `the challenge continues`.

`the challenge is ongoing` is "unobtrusively defined" by `the challenge continues`.

* Whenever `the challenge is ongoing`:
	* `the Customer` may `announce finishing the monster burger` before `challenge endlimit time`, and
	* otherwise `the challenge period is over` if the time reaches `challenge endlimit time`.

`end of the challenge period` is "unobtrusively defined" by `the challenge period is over`.

I won't mention any further "unobtrusive definitions" here.

* Upon `the end of the challenge period`:
	* `the Restaurant` should `check that the customer has finished` within 10 minutes, and
	* otherwise `the Restaurant` `violates its prompt-check guarantee`.

* If
	`the Restaurant` `violates its promp-check guarantee` or
	`the Restaurant` `violates its promp-serve guarantee` or
	`the Restaurant` `confirms the customer is finished`
  	then `amount owing` is set to 0, and the contract is fulfilled.

* Upon `the Restaurant` `having checked that the customer has finished`:
	* `the Restaurant` may `confirm the customer is finished` immediately, or
	* `the Restaurant` may `disconfirm the customer is finished` immediately

* Upon `the Restaurant` `having disconfirmed the customer is finished`:
	* then `the Customer` must `pay the bill` if the time reaches `end of the challenge period`, and
	* otherwise `the challenge continues` if the time is before `end of the challenge period`

* If `the Customer` `pays the bill` then `amount paid` is set to `amount owing` and `amount owing` is set to 0 and the contract is fulfilled.

