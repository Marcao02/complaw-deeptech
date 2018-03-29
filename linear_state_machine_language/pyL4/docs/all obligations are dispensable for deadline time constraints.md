Note that time constraints in L4 are not currently restricted to the form `<TimeDelta term> < last_event_td ≤ <TimeDelta term>`, but I believe all examples can be modified to use only that form, or alternatively this reduction can probably be generalized.

Metavars `P` for party, `Aᵢ` for action, `Bᵢ`,`Eᵢ` for `TimeDelta` expressions, `Wᵢ` for the action parameters guard aka "where clause" (a term of sort `Bool`), `RGᵢ` for rule-enabled guard (a term of sort `Bool`).

Here's an example of the reduction:


```
(Next
	(if RG₁
		(P₁ obligations-options-include A₁ (B₁ < last_event_td ≤ E₁) (where W₁))
	)
	(if RG₂
		(P₂ obligations-options-include A₂ (B₂ < last_event_td ≤ E₂) (where W₂))
	)
)
```

```
(Next
	(if (RG₁ and RG₂)
		(P₁ obligations-options-include A₁ (B₁ < last_event_td ≤ E₁) (where W₁))
		(P₂ obligations-options-include A₂ (B₂ < last_event_td ≤ E₂) (where W₂))
		(Env must EnterBreachP₁P₂ (max(E₁,E₂) < last_event_td))
	)
	(if (RG₁ and (not RG₂))
		(P₁ obligations-options-include A₁ (B₁ < last_event_td ≤ E₁) (where W₁))
		(Env must EnterBreachP₁ (E₁ < last_event_td))
	)
	(if ((not RG₁) and RG₂)
		(P₂ obligations-options-include A₂ (B₂ < last_event_td ≤ E₂) (where W₂))
		(Env must EnterBreachP₂ (E₂ < last_event_td))
	)
)
```