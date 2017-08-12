L4/LSM, for Linear State Machine, is a working name for this language/computation model. A longer but more descriptive name would be Event-Driven Linear State Machine. "Linear" means the computation is single-threaded, which every software engineer knows is good for comprehensibility. The same is true in contracts. It is important to note, though, that this does not actually preclude the modeling of concurrent contracts. With the use of a data structure to, for an example of one kind of simulation, represent the tuple of the current states of all threads, even contracts with lots of concurrency can be simulated by LSM programs of similar size. Parallel efficiency is the main reason we use concurrent programming models in software generally, but for contracts that does not seem to be a concern.
I hope that "state machine" does not make people assume finite state. 

This tutorial will take you through a formalization of a simplification of one of the example informal contracts that Hvitved used in his thesis. It is simplified in that it doesn't include anything about payment. There are two full versions, [one using lists](https://github.com/legalese/legalese-compiler/blob/master/linear_state_machine_language/examples/hvitved_master_sales_agreement_full.LSM) and [the other using sets](https://github.com/legalese/legalese-compiler/blob/master/linear_state_machine_language/examples/hvitved_master_sales_agreement_full_with_ids.LSM). The simplified version is [here](https://github.com/legalese/legalese-compiler/blob/master/linear_state_machine_language/examples/hvitved_master_sales_agreement_simplified.LSM).

L4/LSM unashamedly encourages the use of global vars. They will be ideal for prose contract isomorphism -- consider how infrequently scoped variables are used in English prose (pronouns don't count). They will also be ideal for hooking up to interactive visualization/customization widgets. 

The first two declarations introduce variables that are only for use in monitoring/visualization, testing, and formal verification - hence the `nonoperative` keyword (probably we can come up with a better keyword). The static checker will ensure that they are never read by the contract in a way that influences execution. The keyword `inconly` means the variable only supports the `+=` operation. Currently all types are built-in.

	(GlobalVars 
		( nonoperative inconly units_ordered : ℕ := 0 )
		( nonoperative inconly units_delivered : ℕ := 0 )

*Interlude about the Unicode characters*: I use a Mac/Windows program called TextExpander that makes them easy to input (e.g. `\alpha \in \NN` is replaced by α ∈ ℕ), so I tend to use them often. But of course no language should make them necessary, and they're not necessary in this prototype either. Obviously I'm a fan though. There are many [similar programs](http://alternativeto.net/software/textexpander/). If you adopt one, I'll send you my snippets file.
		
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

The next part defines ids for clauses/sections of the prose contract, so that we can link them to parts of the formal contract. For this example, I have not yet done the linking. [hvitved\_printer\_explicit\_deontic.LSM](https://github.com/legalese/legalese-compiler/blob/master/linear_state_machine_language/examples/hvitved_printer_explicit_deontic.LSM) has linking, but the linking doesn't do anything yet. This text is verbatim from Hvitved's thesis.

	(ProseContract 
	 	(P1 'The master agreement between Vendor and Customer is for 1000 printers, with a unit price of €100. The agreement is valid for one year, starting 2011-01-01.')
	 	(P2 'The customer may at any time order an amount of printers (with the total not exceeding the threshold of 1000), after which the Vendor must deliver the goods before the maximum of (i) 14 days, or (ii) the number of ordered goods divided by ten days.')
	 	(P3 'After delivering the goods, Vendor may invoice the Customer within 1 month, after which the goods must be paid for by Customer within 14 days.'
	)

Finally we get to the formal contract. The formal contract code is partitioned into a finite number of **event states**. These take on all the role of actions in CSL, and more. They are the main, *deliberately-limited means of structuring the code of a contract*, and they are what enable us to convey most of the meaning of a contract in a flow chart. They also yield, as subclasses of the representable contracts, efficiently-queryable classes that are easily recognized. All that said, it is only *recommended*, not *necessary*, to use event states to model as much of the contract state as possible (TODO: Exercise to show that a few event states suffice for any contract). An **action-event state** is an event state that models an action initiated by a specific actor. In contrast, non-action event states are used mostly for external events (e.g. building damaged by fire), including passage of time events (e.g. month ended). 

-------------

Note: because of the design decisions of L4/LSM, there is a simple notion of **comprehensive state**: The state of the contract at any moment in **contract time** is *completely determined* by three things. 

1. The current contract time.
2. One of the finitely-many event states.
3. The values of the global variables. 

There are local variables too, and the `Entrance` computation sections can be long and complex, but since the computation sections happen instantaneously, the values of the local variables in the current event state are determined by the above 3 things.

-------------


After the keyword `FormalContract`, you provide a name. Two other S-expression heads can appear inside the `FormalContract` block: `StartState` and `EventStates`. It probably goes without saying that the `StartState` is the label of the **EventState** where every execution of this contract starts.  

	(FormalContract	
		"Master Sales Agreement"	
		(StartState Start)
		
Next we have the bulk of the formal contract; a list of EventState declarations. At any time during execution, the contract is in one of the event states. For finite state contracts, it is always possible (though often not advisable) to have the entire state of the contract be *(current event state, current time)*. In general, the current contract state is *completely determined* by those two components together with the values of all the global variables. I believe that will be a valuable feature for understandability, visualization/monitoring, testing, and formal verification.
		
Let's look at the first event state now. This one has no **event parameters**; hence the `()` after `Start`. We'll see an event state with a parameter later. If the optional `Entrance` expression is present, as for `Start`, the first thing that happens upon entering the event state is the sequential execution of all the operations in that section. Note that, although the execution is sequential, it happens instantaneously with respect to **contract time** in the semantics of contract execution. Here, there is only one operation, the setting of a global variable (a `writeonce` global variable). In the context of an event state, `contract_start_date` is a special function that gives the date in *contract time* when the event state was entered. There are several other similar such functions, including `contract_start_time`. 
		
		(EventStates
			(Start()
				(Entrance
					(contract_ends := ((contract_start_date) + 365D))
				)
			
The next and last part of the `Start` event state is the `Fallbacks` block. This defines the simplest kind of **transition rule**. Transition rules become active after the execution of `Entrance`. For the special case of the `Fallbacks` block, the rule applies only if all other transition rules (there are none for this event state) are **disabled** (more on this later).
			
				(Fallbacks
					(ContractLive())
				)			
			)

The next event state `ContractLive` is more interesting. It has an `ActorEvents` block (which should perhaps be renamed `Actions`). Each expression inside it defines one or more **action transition rules**. The head of a transition rule is the actor who can/must perform it. The second term is one of the keywords `may, should, must, mayif, shouldif, mustif`. In this case it is `mayif`, which means the transition expression that follows, `(Order(p) by contract_ends where (p ≤ orderable_units))`, is a *permission*, and the `if` part of `mayif` signifies that the immediate next expression `(orderable_units > 0)` is a **deontic guard** (for want of a better name). In the operational semantics, every deontic guard gets evaluated to `true` or `false` immediately after the `Entrance` section has been evaluated. If it evaluates to `false`, then the corresponding permission or obligation is **disabled**, and the transition rule cannot apply. Note that the disabling of an obligation is a relief of the obligation, whereas the disabling of a permission is a revoking of the permission. There is at least (semantics undecided; see next paragraph) one other way that a transition rule can be disabled: when the current time passes the time specified by its **deadline expression** (which we haven't talked about yet, but examples are coming). 

--------------
The third way that a transition rule can be disabled is when its optional **where clause** is unsatisfiable. In this example, it's not possible, because whenever the deontic guard `(orderable_units > 0)` is true, `p = 1` will satisfy the `where` clause. _We will need to decide_, after more experience, whether possibly-unsatisfiable `where` clauses should be allowed, and if so, what they should mean: for example, does an unsatisfiable `where` clause relieve an obligation, or does it cause an immediate failure of the obligation?

--------------

`Order` is an event state (an *action-event state* to be more precise) defined later that takes a parameter (an **event state parameter**). We can introduce a new bound variable (here `p`) to constrain the applicability of the rule based on the parameter value; in this example we specify with `where (p ≤ orderable_units)` that the `Customer` cannot order more units than the number remaining of the 1000 agreed to in the contract terms. 

			(ContractLive()
				(ActorEvents
					(Customer mayif (orderable_units > 0)
						(Order(p) by contract_ends where (p ≤ orderable_units))
					)				
				
The main thing new about the next transition rule is that we use the `where` clause to uniquely specify the acceptable value of the event state parameter of `Deliver`. The "real world" effect is that the `Vendor` must deliver exactly the number of units ordered, in the same sequence as the original orders. They cannot, for example, split the delivery in two. That constraint is lifted in the two other versions of this contract; see links near the top of this document. Our typechecker will verify that if the deontic guard `(nonempty orders)` is true, then `(top orders)` is well defined.
				
					(Vendor mustif (nonempty orders)
						; fst (top orders) is the quantity, snd (top orders) is the deadline
						(Deliver(q) by (snd (top orders)) where (q == (fst (top orders))))
					)
				)

Note `Fulfilled` is a standard name for "the contract completed unbreached":

				(Fallbacks
					; this will only be executed when both of the actions in ActorEvents are disabled.							
					(Fulfilled())
				)
			)

The second event state `Order` declares a parameter, the number of units requested for this order. Next we define the local variable `delivery_deadline` according to the informal contract, which says "Vendor must deliver the goods before the maximum of (i) 14 days, or (ii) the number of ordered goods divided by ten days." 

			(Order(quantity : ℕ)			
				(Entrance
					(delivery_deadline := (days (max 14 (ceil (quantity / 10)))) )
				
We then increment the count `units_ordered` of the total number of units ordered (which, recall, is a `nonoperative` variable). And finally we put the calculated delivery deadline and requested quantity on the bottom of the `orders` queue (recall it's immutable).

The `orders := …` line of code has more of a runtime, operational nature than might be expected in a contract specification; we may benefit from rephrasing this more declaratively, or creating a higher-level construct to encapsulate state in a way that is consistent with isomorphism to English.

					(units_ordered += quantity )
					(orders := (enqueue orders (tuple quantity delivery_deadline)) )
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
	
	(DotFileName 'hvitved_master_sales_agreement_simplified.dot')
	(ImgFileName 'hvitved_master_sales_agreement_simplified.png')
