Symbolic eval, at any time, is in
-A Situation
	-evaluating a precondition or postcondition
	-deciding what ActionRule to apply.
-An Action
	-evaluating the StateTransform section of an Action
	// no need for preconditions and postconditions, since they can just be `assert`s in the StateTransform.

σ is an immutable store for state vars, mapping each to a concrete or typed symbolic value
π is an immutable path constraint
stmt is an immutable (or at least unchanging) Statement
ρ is None or an immutable store for action parameters mapping each to a concrete or typed symbolic value
s is a Situation id
a is None or an Action id
t is the Action or Situation ordinal, which starts at 0 for the first Situation, and is 0 for the first Action.

td is used in variable names for TimeDelta variables.

First:
	Typecheck program
	Eliminate local vars
	Eliminate ifthenelse terms
	Expand `must` expressions


sevalAction(a,σ,π,t,ρ)
	; Note tdₜ was already created at the action rule site.
	sevalBlock(a.statetransform,σ,π,t,ρ,a)
	if a.embeddedSituation
		sevalSituation(a.embeddedSituation,σ,π,t+1,ρ,a)
	else
		sevalSituation(a.transitionsTo,σ,π,t+1,None,None)

sevalBlock(block,σ,π,t,ρ,a):
	Case block empty
		return
	Case block == (stmt block')
		evalStmt(stmt,σ,π,t,ρ,a)
		evalBlock(block',σ,π,t,ρ,a)

sevalStatement(stmt,σ,π,t,ρ,a):

	Case stmt == (x' := e)
		xₜ = new SymbolicVar(x.name, x.type)
		eterm = smtTerm(e)
		σ' = add x_t ↦ eterm to σ.

	Case stmt == (if e block1 else block2)
		eterm = smtTerm(e)
		sevalBlock(block1,σ,and(π,eterm), t,ρ,a)
		sevalBlock(block2,σ,and(π,¬eterm),t,ρ,a)

	Case stmt (assert e)
		eterm = smtTerm(e)
		first check
			z3sat?(σ,ρ,π)
		if false
			then the current path is already inconsistent, so return
		else
			prove(σ,ρ,π |- eterm)


sevalSituation(s,π_in,t,ρ,a):
	; Recall that ρ and a are non-None iff s is an embedded situation (in a)

	tdₜ = new SymbolicVar("tdₜ",ℕ)
	if t ≥ 1
		π = add td_{t-1} ≤ tdₜ to π_in
	else
		π = add tdₜ = 0 to π_in

	for each precondition e
		prove(σ,ρ,π ⊢ smtTerm(e))

	for each action rule (if etest (role may a(V) (where eWhere) (when eWhen)))
		; we pretend etest is `True` if there is no guard

		π' = and(π,smtTerm(etest))
		if not sat?(σ,ρ,π')
			return

		ρ_next = [
			new SymbolicVar(v_{t+1},v.type) if v is an action parameter, or
			smtTerm(v) if v is a Term
		for v in V ]

		if sat?(σ,ρ_next,π')
			π'' = and(π', smtTerm(eWhere), smtTerm(eWhen))
			sevalAction(a,π'',t,ρ_next)
		else
			return

	for each direct transition rule (if etest (a(E) (when eWhen))
		π' = and(π,smtTerm(etest))
		if etest is not `True` and not sat?(σ,ρ,π')
			return
		ρ_next = [ smtTerm(e) for e in E ]

		if sat?(σ,ρ_next,π')
			π'' = and(π', eWhen)
			sevalAction(a,π'',t,ρ_next)