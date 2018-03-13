Welcome to the L4 tutorial. (todo encourage feedback etc)

This will teach you how to use the current implementation of L4, by way of writing a simplified version of Y-Combinator's Simple Agreement for Future Financing. The full version can be found in github (todo link).

Please be aware that you won't be using a carefully-designed concrete syntax. Instead, you'll be using a carefully-designed abstract syntax, which is written in a fully-parenthesized language (like LISP). A nicer looking and slightly-more-concise concrete syntax, with a lot less parentheses, will come later. There are good reasons for delaying it now, which I won't get into. On the other hand, there is one good reason for *not* delaying it, which is that most programmers find fully-parenthesized languages off-putting.

Things this tutorial deliberately *won't* say much about, which will eventually have their own pages:

- Alternative computational legal contract languages, and how and why L4 differs from them.
- The current L4 implementation, pyL4.
- How you can contribute.
- Natural language generation.
- Formal verification features that we haven't implemented yet.

The main components of an L4 contract are:

- **Actors**. Typically, they are the signatories of the contract. When/if an L4 program is ever deployed as a smart contract, then actors correspond to private keys. For now, your L4 contracts can only have a constant number of actors. 
- **Actions**/**Events**. Events are how an L4 contracts gets information about the real world. Actions are events deliberately caused and reported by particular actors (possibly to be disputed by other actors). My sending an e-transfer to you would be modeled as an action. My house getting struck by lightning would be modeled as a non-action event. tutorialSAFE.l4 currently uses only one, non-action event, which is among a few pre-defined events: `Breach_Company`.
- **State**. Real world events change the state of the world. L4 events change the software representation of the (tiny) part of the state of the world that a contract models. That representation is an assignment of data values -numbers, strings, dates, durations, lists, etc- to a set of *state variables*.  
Each state variable has a type, which as usual restricts the set of values it is allowed to have assigned to it. L4 lets you use some types not often found in programming languages, such as currencies, rates (e.g. price of a stock), and some commonly-useful rational number intervals such as (0,1), [0,1), [0,1]. The type system, sometimes with the help of an SMT solver, allows you to prove that all division operations are well defined, that subtraction operations you intend to never result in negative numbers indeed have that property, etc.
- **Action Rules**. These are rules that govern, as a function of the contract state, which actions by which actors may or must be performed next.
- **Situations**. These are, in a a technical sense, a superfluous feature of the language,¹ but they are very useful for organizing large contracts. A coarse but informative view of an execution of an L4 contract (called a **trace**) is a sequence alternating between situation names and action names. A contract defines a finite set of situations. Situations in tutorialSAFE.l4 include `InvestorInvests` and `After_ChooseCashPayment`. The first is named explicitly [see `(Situation InvestorInvests ...)`]. The second has it's name derived from an explicitly-named Action called `ChooseCashPayment` [see `(Action ChooseCashPayment ...)`]. `InvestorInvests` is the *start situation* - the contract always starts there.  

> ¹ An L4 Contract with N `Situation` declarations can always be converted into one with 1 `Situation` and a new state variable whose type is an N-valued Enum... except that I haven't gotten around to implementing Enums.

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

The next section lets you introduce some untyped, argumentless macros. There is a declaration type for macros with arguments as well, but neither tutorialSAFE.l4 nor the full SAFE use them. The one definition we use is part of a temporary hack until we have an `Option` type operator.

	(Definitions
		(VALCAP_STRICT_UPPERBOUND = 9e20)
	)

The next section makes clear that an L4 contract is really a *contract template*. It corresponds to the blanks you need to fill in a normal contract. Each contract parameter has a default value, which of course can and should be overwritten in applications and tests. Note `"Fraction(0,1]"` is another predefined type.² The type of `VALUATION_CAP` should really be `(Option Pos$)`, but I haven't gotten around to implementing the Option type constructor yet.
  
> ² The reason it has quotes around it is that L4's current parser is essentially a generic S-expression parser, and the bracket symbols are among the few symbols that have special meaning in the language of S-expressions].

	(ContractParams
		(PURCHASE_AMOUNT : Pos$ = 100000)
		(VALUATION_CAP : Pos$ = VALCAP_STRICT_UPPERBOUND)
		(DISCOUNT_RATE : "Fraction(0,1]" = 1)
		(START_INVESTOR_COMMON_STOCKS : ShareCnt = 0)
		(START_INVESTOR_SAFE_PREFERRED_STOCKS : ShareCnt = 0)
		(START_INVESTOR_CASH : $ = 0)
	)  

We have a larger section next, which introduces all the state variables, and gives some of them initial values. 

**Note that unicode is never necessary**. You can write `writeAtMostOnce` instead of `writes≤1`, for example.

	(StateVars
		(has_cap : Bool = (VALUATION_CAP < (Pos$ VALCAP_STRICT_UPPERBOUND)))
		(has_discount : Bool = (DISCOUNT_RATE < 1))
	
		(writes≤1 investor_Common_Stocks : ShareCnt = START_INVESTOR_COMMON_STOCKS  )
		(writes≤1 investor_SAFE_Preferred_Stocks : ShareCnt = START_INVESTOR_SAFE_PREFERRED_STOCKS)
		(writes≤1 investor_cash : $ = START_INVESTOR_CASH )
	
		(cash_currently_unconverted : $ = PURCHASE_AMOUNT)
	
		; for Equity Financing
		(writes≤1 safe_price : SharePrice)
		(writes≤1 discount_price : SharePrice)
		(writes≤1 conversion_price : SharePrice)
		(writes≤1 initial_price_per_share_standard_preferred_stock : SharePrice)
	
		; for Liquidity
		(writes≤1 liq_price : SharePrice )
		(writes≤1 liq_cashout : $ = 0 )
		(writes≤1 company_cash : $)       ; maybe this should be in ContractParams.
		(writes≤1 investor_liq_hypothetical_shares : ShareCnt)
		(writes≤1 investor_percent_of_cashout_investor_investments : "Fraction[0,1)")
	
		; for Dissolution
		(writes≤1 dis_cashout : $)
	)














