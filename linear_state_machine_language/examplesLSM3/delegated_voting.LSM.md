**_This algorithm is complete, but not yet in the syntax of LSM3._**

**_PLEASE TELL ME IF YOU WANT TO READ THIS AND I WILL PRIORITIZE MAKING IT SUITABLE FOR READING_**

The contract is created with a fixed set of k proposal, numbered 1,...,k.
Having not voted is represented by 0.

R - the set of **R**egistered voters, which is the disjoint union of the sets U, S, D?, D✓ defined below. For each voter v in R. Each v in R is structure with:

* v.counted ∈ {true,false}, initially false
* v.vote ∈ {0,...,k} where v.vote == 0 means v hasn't voted
* v.delegate ∈ R ⋃ None
* Invariant: v.delegate ≠ v

The state is given by a tuple ❬U, S, D?, D✓, G, P❭:

- U - the set of **U**ndecided voters, who have neither voted nor delegated their vote
	* Invariant v ∈ U → v.vote = 0 and  v.counted = false and v.delegate = None

- S - the set of registered voters who have **S**ubmitted a vote
	* Invariant v ∈ S → v.vote ≠ 0 and v.counted = true and v.delegate = None

- D? - the set of registered voters who have **D**elegated their vote, whose delegated vote is not yet decided.
	* Invariant v ∈ D? → v.delegate ≠ None and v.counted = false and v.vote = 0

- D✓ - the set of registered voters who have **D**elegated their vote, whose delegated vote has been decided. 
	* Invariant v ∈ D✓ → v.delegate ≠ None and v.vote = 0

- G - A directed graph on R, where:
	* the U and S voters have no out-edges
	* the D voters have exactly one out-edge, and any number of in-edges
	* The subgraph obtained by ignoring all out-edges from nodes in D? is a forest. Thus, among confirmed delegated voters, there are no cycles. 

- P - the set of k proposals to be voted on. 
	* Each p ∈ P has p.votecnt ∈ ℕ
	* Each p ∈ P initially has p.votecnt = 0

	
**Algorithm**

* _Action_ (Submit): An undecided voter v submits a vote for proposal p.
	* Pre: v ∈ U
	* Post:
		* U := U - v
		* S := S + v
		* v.vote := p
		* p.votecnt += 1
		* u.counted := true
		
* _Action_ (Delegate): An undecided voter u delegates to another, different voter v.
	* Pre: u ∈ U, v ∈ R, u ≠ v
	* Post:
	 	* u.delegate := v
		* U := U - u		
		* D? := D? + u

* _Action_ (Cancel delegation and vote - *this is optional*): If u has delegated to v and is unconfirmed, and v is in U or D?, then u can cancel their delegation and vote directly for a proposal p. 
	* Pre: u ∈ D?, v ∈ U ⋃ D? 
	* Post:
	 	* u.delegate := None
		* D? := D? - u
		* S := S + u
		* u.vote := p
		* p.votecnt += 1
		* u.counted := true		

* _Action_ (Redeligate): If u has delegated to v and is unconfirmed, and v has not participated or also has an unconfirmed delegation, then u can change their delegation to another voter, provided that voter either directly voted or has a confirmed delegation. This allows for two things: (1) breaking cycles, and (2) giving a voter one more (less flexible) chance to redeligate their vote if the person they originally delegated to hasn't voted.
	* Pre: u ∈ D?, v ∈ U ⋃ D?, w ∈ S ⋃ D✓, {u, v, w} all distinct
	* Post:
	 	* u.delegate := w
		* D? := D? - u
		* D✓ := D✓ + u

* _Nonaction State Transition_ (Count): u has (confirmed or unconfirmed) delegation to v, and v has submitted their vote, and u has not had their vote counted, then this transition count's u's vote, and marks u's delegation as confirmed if it isn't already.
	* Pre: u ∈ D? ⋃ D✓, v ∈ S, u.counted = false
	* Post:
		* v.vote.votecnt += 1
		* u.vote := v.vote
		* u.counted := true
		* D? := D? - u 
		* D✓ := D✓ + u
					
* _Nonaction State Transition_ (Shorten delegation chain): If u has an unconfirmed delegation to a voter v, where v delegates to a voter w who has submitted, then we move u's delegation from v to w. 
	* Pre: u ∈ D?, v ∈ D✓, w ∈ S, u.delegate = v, v.delegate = w
	* Assert u,v,w distinct
	* Post:
		* u.delegate := w
		* D? := D? - u
		* D✓ := D✓ + u

		
#### Optimization:
In a smart contract, the two kinds of Nonaction State Transitions can be carried out repeatedly until the gas is spent or the block is full.

#### Correctness
Todo

<!--
* _Nonaction State Transition_: u has unconfirmed delegation to v, but v ∈ D✓ was already delegated to u. Then we reject u's delegation.
	* Pre: u ∈ D?, v ∈ D✓, u.delegate = v, v.delegate = u
	* Post:
		* u.delegate := None
		* D? := D? - u
		* U := U + u
-->
		
<!--	
* Nonaction State Transition: u has submitted their vote, but hasn't had it counted
	* Pre u ∈ S and u.weight > 0
	* Post:
		* (u.vote.votecnt, u.weight) := (u.vote.votecnt + u.weight, 0)
		-->