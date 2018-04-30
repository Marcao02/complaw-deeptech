Not all of these types appear in pyL4. This document is to help you answer detailed questions about the mathematical model. Fix an L4 contract.  

Let x₁:T₁, …, xₙ:Tₙ be its state variables and their types.  

Let a₁:D₁, …, aₖ:Dₖ be the `Action`s and their parameter types (Dᵢ is a tuple if aᵢ has more than one parameter).  

Let Situation be the simple enum type of the names of all the `Situation`s. That includes the explicitly-defined ones, together with some that are automatically defined in terms of other things, e.g. After\_‹Action name›.
  
Let ContractState be the type  
Tuple Situation TimeDelta {x₁:T₁, …, xₙ:Tₙ}
where the part in {} is a record type.

Let ActionInstance be the algebraic datatype  
a₁ of D₁ | … | aₖ of Dₖ.

Let Trace be  
Tuple ContractState DateTime (Sequence ActionInstance)  
Then Trace is a model of the well-typed traces of the contract. Note that some of the well-typed traces will violate the contract's rules.

Let τ be a Trace.  
τ[0] is the start state, whose TimeDelta component is 0  
τ[1] is the absolute start time. This is only used to map the TimeDeltas appearing in the program to DateTimes.  
Let α₁,...,αₜ be τ[2].  
A trace determines a sequence of ContractStates s₀,s₁,... starting from s₀ = τ[0]. Applying ActionInstance αᵢ to ContractState s_{i-1} yeilds sᵢ.



