


This will teach you how to use the current implementation of L4, by way of writing a simplified version of Y-Combinator's Simple Agreement for Future Financing. The full version can be [found in github](https://github.com/legalese/complaw-deeptech/blob/master/linear_state_machine_language/pyL4/examples/src_sexpr/serious/SAFE.l4). The version used here is obtained from the full version by:

- setting two preprocessor flags (a feature not covered here) `HAS_CAP` and `HAS_DISCOUNT` to true.
- expanding instances of the one macro defined in SAFE.l4.
- deleting `Situation`, `Action` and action rule declarations corresponding to two of the four categories of ways that the Company can fulfill the contract.
- setting some deadlines to 7 or 14 days, arbitrarily, where they are either (a) not specified in the SAFE (and thus taken to be something a judge would use their discretion on), or (b) governed by the following clause:

	> (b)	Any notice required or permitted by this instrument will be deemed sufficient when delivered personally or by overnight courier or sent by email to the relevant address listed on the signature page, or 48 hours after being deposited in the U.S. mail as certified or registered mail with postage prepaid, addressed to the party to be notified at such party’s address listed on the signature page, as subsequently modified by written notice.

- Removing some state variable annotations, which direct the static checker to verify certain properties. In the full version, most of the state variables have a `writes≤1` annotation.

Please be aware that you won't be using a carefully-designed concrete syntax. Instead, you'll be using a carefully-designed abstract syntax, which is written in a fully-parenthesized language (like LISP). A nicer looking and slightly-more-concise concrete syntax, with a lot less parentheses, will come later. There are good reasons for delaying it now, which I won't get into. On the other hand, there is one good reason for *not* delaying it, which is that most programmers find fully-parenthesized languages off-putting.

Things this tutorial deliberately *won't* say much about, which will eventually have their own pages:

- Alternative computational legal contract languages, and how and why L4 differs from them.
- The current L4 implementation, pyL4.
- How you can contribute.
- Natural language generation.
- Formal verification features that we haven't implemented yet.

The main components of an L4 contract are:

- **Actors**. Typically, they are the signatories of the contract. When/if an L4 program is ever deployed as a smart contract, then actors correspond to private keys. For now, your L4 contracts can only have a constant number of actors.
- **Actions**/**Events**. Events are how an L4 contracts gets information about the real world. Actions are events deliberately caused and reported by particular actors (possibly to be disputed by other actors). My sending an e-transfer to you would be modeled as an action. My house getting struck by lightning would be modeled as a non-action event. tutorialSAFE.l4 currently uses only one, non-action event, which is among a few pre-defined events: `Breach_Company`. Actions may have parameters, e.g. the amount sent in the e-transfer.
- **Situations**. These are, in a a technical sense, a superfluous feature of the language (you'll see why soon), but they are very useful for organizing large contracts. A contract defines a finite set of situations. The situations in tutorialSAFE.l4 include `InvestorInvests` and `After_ChooseCashPayment`. The first is named explicitly [see `(Situation InvestorInvests ...)`], whereas the second has it's name and properties derived from an explicitly-named Action called `ChooseCashPayment` [see `(Action ChooseCashPayment ...)`].
`InvestorInvests` is the *start situation* - the contract always starts there.
- **State**. Real world events change the world state. L4 events change the software representation of the (tiny) part of the world state that a contract models. That representation is composed of three parts:

	1. An assignment of data values (numbers, strings, dates, durations, lists, etc) to a set of **state variables**
	2. The current time duration since the start of the contract.
	3. The current `Situation` (see below).

	Each state variable has a type, which as usual restricts the set of values it is allowed to have assigned to it. L4 lets you use some types not often found in programming languages, such as currencies, rates (e.g. price of a stock), and some commonly-useful rational number intervals such as (0,1), [0,1), [0,1]. The type system, sometimes with the help of an SMT solver, allows you to prove that all division operations are well defined, that subtraction operations you intend to never result in negative numbers indeed have that property, etc.
- **Action Rules**. These are rules that govern, as a function of the contract state, which actions by which actors may or must be performed next. Each action rule belongs to a unique `Situation`.

[//]: # "A coarse but informative view of an execution of an L4 contract (called a **trace**) is a sequence alternating between situation names and action names."
[//]: # "> An L4 Contract with N `Situation` declarations can always be converted into one with one `Situation` and a new state variable whose type is an N-valued Enum... except that I haven't gotten around to implementing Enums."

Let's go into tutorialSAFE.l4 now.

The first line declares the set of actors, and the second line tells us that the smallest time unit the contract cares about is days. The other values supported are: weeks, hours, minutes, seconds.

	(Actors Investor Company)
	(TimeUnit days)

Next, we use some predefined types (`NonnegReal, PosReal, Nat, PosInt`) and some predefined type operators (`Dimensioned, Ratio`) to introduce the types of data that this contract is concerned with: money amounts, share counts, and share prices. They are all nonnegative. We also introduce strictly-positive versions of them: they come in handy for proving that division is well defined and stuff like that, but more importantly they allow us the more precisely express our intent.

	(TypeDefinitions
		($ := (Dimensioned NonnegReal "$"))
		(Pos$ := (Dimensioned PosReal "$"))

		(ShareCnt = (Dimensioned Nat "shares"))
		(PosShareCnt = (Dimensioned PosInt "shares"))

		(SharePrice = (Ratio $ PosShareCnt))
		(PosSharePrice = (Ratio Pos$ PosShareCnt))
	)

The next section makes clear that an L4 contract is really a *contract template*. It corresponds to the blanks you need to fill in a normal contract. Each contract parameter is a constant at runtime and has a default value, which of course can and should be overwritten in applications and tests. `"Fraction(0,1]"` is another predefined type, for the rational interval (0,1].

[//]: # "² The reason it has quotes around it is that L4's current parser is essentially a generic S-expression parser, and the bracket symbols are among the few symbols that have special meaning in the language of S-expressions]."

	(ContractParams
		(PURCHASE_AMOUNT : Pos$ = 1e4)
		(VALUATION_CAP : Pos$ = 1e6)
		(DISCOUNT_RATE : "Fraction(0,1]" = .95)
		(START_INVESTOR_COMMON_STOCKS : ShareCnt = 0)
		(START_INVESTOR_SAFE_PREFERRED_STOCKS : ShareCnt = 0)
		(START_INVESTOR_CASH : $ = 0)
	)

We have a larger section next, which introduces all the state variables, and gives some of them initial values.

[//]: # "There are keywords that you can prepend to a variable declaration, which do not change how the contract executes, but instructs the formal verification engine to verify certain properties. In this contract, most execution `writes≤1`, `branchUnaffecting`, `writeOnce`"
[//]: # "**Note that unicode is never necessary**; you can write `writeAtMostOnce` instead of `writes≤1`, for example."


	(StateVars
		(investor_Common_Stocks : ShareCnt = START_INVESTOR_COMMON_STOCKS  )
		(investor_SAFE_Preferred_Stocks : ShareCnt = START_INVESTOR_SAFE_PREFERRED_STOCKS)
		(investor_cash : $ = START_INVESTOR_CASH )

		(cash_currently_unconverted : $ = PURCHASE_AMOUNT)

		; for Equity and Liquidity
		(conversion_price : SharePrice)
		(cap_price : SharePrice)
		(discount_price : SharePrice)
		(undiscounted_price_per_share_standard_preferred_stock : SharePrice)

		; for Liquidity
		(liq_cashout : $ = 0 )
		(investor_liq_hypothetical_shares : ShareCnt)
		(investor_percent_of_cashout_investor_investments : "Fraction[0,1)")
	)


We have a first taste of beyond-typechecking formal verification next. pyL4 can generate SMT instances to prove that this property holds in the beginning, and that it is maintained by every `Action`'s `StateTransform`.

	(Invariants
		(liq_cashout ≤ cash_currently_unconverted)
	)

[//]: # "There's a simple macro system. We use one to compute the share price that de"

Most of the program is in the section named `Actions&Situations` (or Situations&Actions). The string after that keyword is a name used for the program in some situations. We already mentioned the role of `StartSituation`.


	(Actions&Situations "Reduced SAFE for tutorial"
		(StartSituation InvestorInvests)

Now let's look at the first `Situation` definition. It tells us that the first action in an execution of this contract must be by `Company`. This tutorialSAFE.l4 excludes two of the action rules from SAFE.l4. The ones remaining correspond to starting a new round of funding (`CommitToEquityFinancing`), and to ending the private fundraising by doing an Initial Public Offering. The parameter `?1` in `(CommitToIPO ?1)` corresponds to the one action parameter of `CommitToIPO`. It is for constraining the allowed values of the parameter, although this action rule doesn't do so. If we were to add `(where (?1 ≤ 1000))`, the (nonsensical) effect in this contract would be that there must be at most 1000 existing shares of the company's stock for the company to do an IPO.
You can also see here that `;` starts a line comment in L4.

	(Situation InvestorInvests
		(Next
			(Company may CommitToEquityFinancing)
			(Company may (CommitToIPO ?1))
			; Should Company breach after a while?
			; No - SAFE says explicitly that they can
			; remain in this state forever.
		)
	)

Next we have the first `Action` declaration. After the company registers that they are doing an equity financing by posting this action, they must within 7 days send to the investor the relevant transaction documents. The `TimeUnit` of a contract determines the default time unit, which is days in this case, and so `7d` in the below can just be written `7`. We will wait till the next `Action` declaration (`SendEFTransactionDocsWithPRRA`) to talk about the meaning of `FollowingSituation`.

	(Action CommitToEquityFinancing (AllowedSubjects Company)
		(FollowingSituation
			(Next
				(Company must SendEFTransactionDocsWithPRRA (within 7d))
			)
		)
	)

The above `must` action rule is *exactly* equivalent to the following pair of a `may` action rule and a _simple deterministic transition rule_, which is a kind of non-action event rule:

			(Next
				(Company may SendEFTransactionDocsWithPRRA (within 7))
				(Breach_Company (after 7))
			)

`(within T)` and `(after T)` are also abbreviations for commonly used patterns:

- `(within T)` = `(when (next_event_td ≤ (last_event_td + T))`
- `(after T)` = `(when (next_event_td = (last_event_td + T + 1))`

Whenever the `must` abbreviation is used, the `when` clause should have such a simple "deadline" form. This is because of the nature of how simple deterministic transition rules "fire"; they fire immediately when the `when` clause becomes true. Generally, `may` action rules can have more complex `when` clauses, for example saying that an action is permitted on any Monday in the future.

The next two `Action`s are very similar to `CommitToEquityFinancing`, except that `ExecuteAndDeliverTransactionDocs` is an investor-only action.   `FollowingSituation` is more shorthand, which can only be used inside an `Action` definition. The following instance of the form implicitly creates a `Situation` named `After_SendEFTransactionDocsWithPRRA`, which is immediately entered after the action updates the contract's state variables. For this action, no state variable updates happen, but we will see an example next.

[//]: # "The quantitative properties of these actions is trivial, but they will compile to non-trivial natural language clauses."

	(Action SendEFTransactionDocsWithPRRA (AllowedSubjects Company)
		(FollowingSituation
			(Next
				(Investor must ExecuteAndDeliverEFTransactionDocs (within 14))
			)
		)
	)

	(Action ExecuteAndDeliverEFTransactionDocs (AllowedSubjects Investor)
		(FollowingSituation
			(Next
				(Company must (IssueSAFEPreferredStock ?1 ?2) (within 14))
			)
		)
	)

There is more going on in the next `Action`.


	(Action (IssueSAFEPreferredStock
				(company_capitalization : PosShareCnt)
				(premoney_valuation : Pos$) ) (AllowedSubjects Company)
		(StateTransform
			(cap_price' = (VALUATION_CAP / company_capitalization))
			(undiscounted_price_per_share_standard_preferred_stock' = (premoney_valuation / company_capitalization))
			(discount_price' = (undiscounted_price_per_share_standard_preferred_stock' * DISCOUNT_RATE))
			(conversion_price' = (min cap_price' discount_price'))
			(investor_SAFE_Preferred_Stocks' += (PURCHASE_AMOUNT round/ conversion_price'))
		)
		(FollowingSituation
			(Next (Company must DoEquityFinancing (within 1)))
		)
	)



