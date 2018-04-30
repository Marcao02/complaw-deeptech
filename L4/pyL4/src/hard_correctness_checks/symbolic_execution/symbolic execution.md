Symbolic eval, at any time, is in
-A Situation
	-evaluating a precondition or postcondition
	-deciding what ActionRule to apply.
-An Action
	-evaluating the StateTransform section of an Action
	// no need for preconditions and postconditions, since they can just be `assert`s in the StateTransform.

First:
	Typecheck program
	Eliminate local vars
	Eliminate ifthenelse terms
	Expand `must` expressions
	Send L4Contract through term2

Anything completely fixed for an L4Contract program is not represented symbolically here. It will be represented in class variables that don't change. This includes the types of variables.

SymbolicTerm is a term in the usual sense but all of whose variables are symbolic.

A Path is an ActionEvalPath or a BlockEvalPath.
Jobs is a collection of Paths.

⌘ Idea: Explore paths in terms of the speed or success rate of SMT solver so far along that path. This means we will get as much as we can out of that solver.

toTermSMT : Term -> TermSMT
toFormulaSMT : Term -> FormulaSMT

Each var in a TermSMT is either
(1) a symbolic var for an inital StateVar value
(2) a symbolic var for an action parameter for the t-th action, for some t.

From "partial path" that
	stops at stmt
	has map σ of statevars to TermSMTs, for current statevar vals
	has map σ' of statevars to TermSMTs, for next statevar vals
	has map ρ of current action's params to TermSMTs, for vals of the params
	has a formula π generated from the nondeterministic choices needed to get here
That partial path will eventually generate multiple extensions of itself.



sevalStmt(  stmt:Statement,
			σ:FrozenDict[str,TermSMT],
			σ':Dict[str,TermSMT],
			ρ:Dict[str,TermSMT],
			π:FormulaSMT ) -> List[Path]
	case stmt == (x' := e)
		σ'[x] = toTermSMT(e)
		return ??

	case stmt == (if e then b1 else b2)
		estmt = toFormulaSMT(e)
		symbolic_nextvals = freeze(σ')
		add to paths (path-jobs) GuardedBlockEvalPath(estmt,b1,σ,symbolic_nextvals,ρ,π) # some of these args aren't necessary I think
		add to paths (path-jobs) GuardedBlockEvalPath(¬estmt,b2,σ,symbolic_nextvals,ρ,π)

	case assert ...


sevalBlock( block:List[Statement,
			σ:FrozenDict[str,TermSMT],
			σ':Dict[str,TermSMT],
			ρ:Dict[str,TermSMT],
			π:FormulaSMT ) -> List[Path] )
	case block == stmt :: restblock
		paths = sevalStmt( stmt, σ, σ', ρ, π )
		paths2 = []
		for path in paths:
			for continuation in sevalBlock(block, σ, path.σ', ρ, π)
				paths2.append( path + continuation )
		return paths2

	case block == []
		return ??

# returns list of partial paths obtained by executing current partial path on this state transform section, and then moving to the next Situation
sevalAction(a:Action,
			σ:FrozenDict[str,TermSMT],
			ρ:Dict[str,TermSMT],
			π:FormulaSMT ) -> List[SituationEvalPath]
	paths = sevalBlock(a.statetransform, σ, {}, ρ, π)
	rv = []
	for path in paths
		if a.embeddedSituation
			for path2 in sevalSituation(a.embeddedSituation,σ,π,t+1,σ',a):
				rv.append(path + path2)
		else
			for path2 in sevalSituation(a.transitionsTo,σ,π,t+1,None,None)
				rv.append(path + path2)


sevalSituation( s:Situation,
	 			)




Γstate maps symbolic state var names to SMTLIB types. No need for a SymbolicVar type.
Γact is similar but for action params. Thus, these don't actually change during a StateTransform eval.
σs is a function mapping state variable
π is an immutable path constraint
stmt is an immutable (or at least unchanging) Statement
s is a Situation id
a is None or an Action id
t is the Action or Situation ordinal, which starts at 0 for the first Situation, and is 0 for the first Action.

td is used in variable names for TimeDelta variables.



sevalAction(a,Γstate,π,t,Γact)
	; Note tdₜ was already created at the action rule site.
	sevalBlock(a.statetransform,Γstate,π,t,Γact,a)
	if a.embeddedSituation
		sevalSituation(a.embeddedSituation,Γstate,π,t+1,Γact,a)
	else
		sevalSituation(a.transitionsTo,Γstate,π,t+1,None,None)

sevalBlock(block,Γstate,π,t,Γact,a) -> StateStore,PathForm:
	Case block empty
		return (empty,empty)

	Case block == (stmt block')
		Γstate',π' = evalStmt(stmt,Γstate,π,t,Γact,a)
		return evalBlock(block',Γstate',π',t,Γact,a)

sevalStatement(stmt,Γstate,π,t,Γact,a) -> StateStore,PathForm:

	Case stmt == (x' := e)
		xₜ = new_symbvar_name(x.name, t) # checks to make sure it's the first time
		Γstate' = fdict_set(Γstate, xₜ, smtType(x.type))
		π' = and(π, (xₜ == term2term2smtterm(e)))
		return (Γstate',π')

	Case stmt == (if e block1 else block2)
		eterm = term2smtterm(e)
		; recall this that no statement follows an (if else) statement,
		; so we can ignore the return value.
		Add to Jobs
			BlockEval(block1,Γstate,and(π,eterm),     t,Γact,a)
			BlockEval(block2,Γstate,and(π,not(eterm)),t,Γact,a)

	Case stmt (assert e)
		eterm = term2smtterm(e)
		first check
			z3sat?(Γstate,Γact,π)
		if false
			then the current path is already inconsistent, so return
		else
			prove(Γstate,Γact,π |- eterm)


sevalSituation(s,π_in,t,Γact,a):
	; Recall that Γact and a are non-None iff s is an embedded situation (in a)

	tdₜ = new SymbolicVar("tdₜ",ℕ)
	if t ≥ 1
		π = add td_{t-1} ≤ tdₜ to π_in
	else
		π = add tdₜ = 0 to π_in

	for each precondition e
		prove(Γstate,Γact,π ⊢ term2smtterm(e))

	for each action rule (if etest (role may a(V) (where eWhere) (when eWhen)))
		; we pretend etest is `True` if there is no guard

		π' = and(π,term2smtterm(etest))
		if not sat?(Γstate,Γact,π')
			return

		Γact_next = [
			new SymbolicVar(v_{t+1},v.type) if v is an action parameter, or
			term2smtterm(v) if v is a Term
		for v in V ]

		if sat?(Γstate,Γact_next,π')
			π'' = and(π', term2smtterm(eWhere), term2smtterm(eWhen))
			Add to Jobs:
				ActionEval(a,π'',t,Γact_next)
		else
			return

	for each direct transition rule (if etest (a(E) (when eWhen))
		π' = and(π,term2smtterm(etest))
		if etest is not `True` and not sat?(Γstate,Γact,π')
			return
		Γact_next = [ term2smtterm(e) for e in E ]

		if sat?(Γstate,Γact_next,π')
			π'' = and(π', eWhen)
			Add to Jobs:
				ActionEval(a,π'',t,Γact_next)