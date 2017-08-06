L4/LSM, for Linear State Machine, is a working name for this language/computation model. A mouthful but more descriptive name for would be Event-Driven Linear State Machine. I hope that "state machine" does not make people think finite state.

This tutorial will take you through a formalization of a simplification of one of the example informal contracts that Hvitved used in his thesis. It is simplified in that it doesn't include anything about payment. I'll add a link to a full version once it's done. The simplified version is  **[here](https://github.com/legalese/legalese-compiler/blob/master/linear_state_machine_language/examples/hvitved_simplified_master_sales_agreement_explicit_deontic.LSM).**

L4/LSM unashamedly encourages the use of global vars. They will be ideal for prose contract isomorphism -- consider how infrequently scoped variables are used in English prose (pronouns don't count). They will also be ideal for hooking up to interactive visualization/customization widgets.

The first two declarations introduce variables that are only for use in monitoring/visualization, testing, and formal verification - hence the `nonoperative` keyword (probably we can come up with a better keyword). The static checker will ensure that they are never read by the contract in a way that influences execution. The keyword `inconly` means the variable only supports the `+=` operation. Currently all types are built-in.

	(GlobalVars 
		( nonoperative inconly units_ordered : ℕ := 0 )
		( nonoperative inconly units_delivered : ℕ := 0 )

*Interlude about the Unicode characters*: I use a Mac/Windows program called TextExpander that makes them easy to input (e.g. `\alpha \in \NN` is replaced by α ∈ ℕ), so I tend to use them often. But of course no language should make them necessary, and they're not necessary in this prototype either. Obviously I'm a fan though. These are many [similar programs](http://alternativeto.net/software/textexpander/). If you adopt one, I'll send you my snippets file.
		
The next variable `contract_ends` is declared `writeonce`. The static checker will verify that it is assigned to exactly *twice* in every possible contract execution. Just kidding exactly once.

		( writeonce contract_ends : Date )
		
The next variable is of a type that I first introduced for formalizing this contract. Currently I'm thinking we should make all types immutable, so that `(enqueue x orders)` and `(dequeue orders)` are different objects. The current usage of `orders` reflects that. The object `emptyQueue()` belongs to type `Queue[T]` for all types `T`.
		
		( orders : Queue[Pair[ℕ,Deadline]] := emptyQueue() ) 
	)
	
The next declaration is pretty self explanatory.

	(Actors Customer Vendor)

`ContractParams` are constants at runtime which we might want to set differently in testing or different uses of the contract.

	(ContractParams 
		(MAX_UNITS : ℕ := 1000)
		(CONTRACT_LIFE : Date := 1Y)
	)
	
Note that I've adopted Hvitved's syntax `1Y`, `1M`, `5D` etc, but I think we should use a more sophisticated semantics. In his thesis, `1M = 30D = 720H`, for example, and `1-Sept-2017 + 1M = 31-Sept-2017`. I think adding `1M` to a Date should be an error. Instead, we'd use `(firstDayOf (nextMonth 1-Sept-2017))` which equals 1-Oct-2017.

The next bit is pretty self-explanatory. A reactive T variable is like a 0-argument function (in an impure language) that returns a T, except that its type is T, not a function type.

	(ReactiveVars
		(orderable_units : ℕ := (MAX_UNITS - units_ordered))
	)

The next part defines ids for clauses/sections of the prose contract, so that we can link them to parts of the formal contract. For this example, I have not yet done the linking. `hvitved_printer_explicit_deontic.LSM` has linking, but the linking doesn't do anything yet. This text is verbatim from Hvitved's thesis.

	(ProseContract 
	 	(P1 'The master agreement between Vendor and Customer is for 1000 printers, with a unit price of €100. The agreement is valid for one year, starting 2011-01-01.')
	 	(P2 'The customer may at any time order an amount of printers (with the total not exceeding the threshold of 1000), after which the Vendor must deliver the goods before the maximum of (i) 14 days, or (ii) the number of ordered goods divided by ten days.')
	 	(P3 'After delivering the goods, Vendor may invoice the Customer within 1 month, after which the goods must be paid for by Customer within 14 days.'
	)
	
Finally we get to the formal contract. After the keyword `FormalContract`, you provide a name. Two other S-expression	heads can appear inside the `FormalContract` block: `StartState` and `EventStates`. Probably goes without saying: the `StartState` is the label of the EventState where every execution of this contract starts.

	(FormalContract	
		"Master Sales Agreement"	
		(StartState Start)
		
Next we have the bulk of the formal contract; a list of EventState declarations. At any time during execution, the contract is in one of the event states. For finite state contracts, it is always possible (though often not advisable) to have the entire state of the contract be *(current event state, current time)*. In general, the current contract state is **completely determined** by those two components together with the values of all the global variables. I believe that will be a valuable feature for understandability, visualization/monitoring, testing, and formal verification.
		
Let's look at the first event state now. This one has no **event parameters**; hence the `()` after `Start`. We'll see an event state with a parameter later. If the optional `Entrance` expression is present, as for `Start`, the first thing that happens upon entering the event state is the sequential execution of all the operations in that section. Note that, although the execution is sequential, it happens instantaneously with respect to "real time" in the semantics of contract execution. Here, there is only one operation, the setting of a global variable (a `writeonce` global variable). In the context of an event state, `contract_start_date` is a special function that gives the "real time" date when the event state was entered. `contract_start_time` is similar. 
		
		(EventStates
			(Start()
				(Entrance
					(contract_ends := (contract_start_date() + 365D))
				)
			
The next and last part of the `Start` event state is the `Fallbacks` block. This defines an **event state transition** that happens after the execution of `Entrance` and after all other transitions (there are none for this event state) have been **disabled** (either by having their deadline passed or their **deontic guard** falsified; more on this later.)
			
				(Fallbacks
					(ContractLive())
				)			
			)

The next event state is more interesting. It has an `ActorEvents` block (which should perhaps be renamed `Actions`). Each expression inside it defines one or more **action transition rules**. The head of a transition rule is the actor who can/must perform it. The second term is one of the keywords `may, should, must, mayif, shouldif, mustif`. In this case it is `mustif`, which means the transition expression that follows (`(Order(_) by contract_ends)`) is a *permission*, and the `if` part of `mustif` means that the immediate next expression `(orderable_units > 0)` is a **deontic guard**. In the operational semantics, every deontic guard gets evaluated to `true` or `false` immediately after the `Entrance` section has been evaluated. If it's `false`, then the permission or obligation gets disabled.

`Order` is an event state (an **action-event state** to be more precise) that takes a parameter (an **event state parameter**). The syntax `(_)` indicates that we don't want to put any constraints on the value of that parameter. Currently, the only such constraint available is to uniquely specify what the value of that parameter must be, which we do in the next transition rule (thus we can currently say exactly what a parameter must be, or we can say that it can be anything the types allow). This I'm sure will change in the near future, probably, as Meng suggested, in concert with a change/removal of to the guarded deontic keywords.

			(ContractLive()
				(ActorEvents
					(Customer mayif ((orderable_units > 0) and (q ≤ orderable_units))
						(Order(q) by contract_ends)
					)				
				
The main thing new about the next transition rule is that we use the deontic guard (expression following `mustif`) to uniquely specify the acceptable value of the event state parameter of `Order`. The 'real world' effect is that the Vendor must deliver exactly the number of units ordered. They cannot, for example, split the delivery in two. Generalizing this contract to allow for that might be a good exercise. Our typechecker will verify from `(nonempty orders)` in the guard that if this transition is used, then `(top orders)` is well defined.
				
					(Vendor mustif ((nonempty orders) and (q == (fst (top orders))))
						; fst (top orders) is the quantity, snd (top orders) is the deadline
						(Deliver(q) by (snd (top orders)))
					)
				)
							
Note `Fulfilled` is a standard name for "the contract completed unbreached":

				(Fallbacks
					; this will only be executed when both of the actions in ActorEvents are disabled.							
					(Fulfilled())
				)
			)

The second event state `Order` takes a parameter, the number of units requested for this order. Next we set the local variable `delivery_deadline` according to the informal contract, which says "Vendor must deliver the goods before the maximum of (i) 14 days, or (ii) the number of ordered goods divided by ten days." 

			(Order(quantity : ℕ)			
				(Entrance
					(local delivery_deadline := contract_start_date() + (max 14D ((ceil (quantity/10))D))
				
We then increment the count `units_ordered` of the total number of units ordered (which, recall, is a `nonoperative` variable). And finally we put the calculated delivery deadline and requested quantity on the bottom of the `orders` queue (recall it's immutable).				

					(units_ordered += quantity )
					(orders := (enqueue orders (quantity delivery_deadline)) )
				)	

We then immediately return to the main `ContractLive` event state, from which another order can be made, or `Vendor` can do a `Deliver` action-event. The `Deliver` event state doesn't introduce anything new, so I won't comment further on it.

				(Fallbacks
					(ContractLive())
				)
			)
	
			(Deliver(quantity : ℕ)
				(Entrance
					(units_delivered += quantity)
					(orders := (discardTop orders))
				)
				(Fallbacks
					(ContractLive())
				)
			)
		)
	)		
	
	(DotFileName 'hvitved_master_sales_agreement.dot')
	(ImgFileName 'hvitved_master_sales_agreement.png')