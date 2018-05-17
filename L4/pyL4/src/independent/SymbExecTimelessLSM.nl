; In Python
; ✓ 1 parse as s-expression (using existing code!! can replace later if get first version finished!!)
; ✓ 2 expand macros (easyish)
	; ✓ 2a collect macros
	; ✓ 2b do recursive expansion in macro bodies, bottom-up
	; ✓ 2c expand macros in main program
; ✓ 3 replace **, and overloads (easy, cuz for now same name requires different # params)
; ✓	3.5 collect tns names
; ✓ 4 create dotcall nodes.
;	(W . fnsymb Y Z) becomes (.fnsymb W Y Z)
;	(namespace . fnsymb Y Z) becomes (namespace.fnsymb Y Z)
;	(W . fnsymb (Y . fnsymb2 Z)) becomes (.fnsymb W (.fnsymb2 Y Z))
;	((W . fnsymb Y) . fnsymb2 Z) becomes (.fnsymb2 (.fnsymb W Y) Z))
;	(W1 W2.fnsymb Y1 Y2.fnsymb2 Z1 Z2) becomes (.fnsymb2 (.fnsymb W Y) Z))
; 5 typecheck and insert coercions
	; Terms to typecheck are all inside (`fn`...) nodes, and come in these forms
	; (1)	((...) ~> Term), only in function arg position (not in function position itself)
	; (2)	((...) ~> Block), only in function arg position (not in function position itself)
	;		Should I try to eliminate this form? Or insert `block`?
	; (3)	(Namespace.fnsymb ...)
	; (4)	(.fnsymb x ...)
	; (5)	(`fn`-defined-function-symb ...)
	; 5a Need some way of assigning NL types to subterms.
; 6 eliminte dot call syntax in favor of Namespace.fn form (even in languages with dynamic dispatch!)
; 7 hard-coded routine for expanding into some mainstream language


(tns Void)

(macro upcast (T) (absfn @upcastToT (**) -> T))

(tns Id)

(macro TypeOfId (T)
	(tns TId
		(upcast Id)
	)
)
(TypeOfId Sort) ; or, if use Racket: (TypeOfId Sort SortId)
(TypeOfId SortOp)
(TypeOfId Agent)
(TypeOfId EventParam)
(TypeOfId FnSymb)
(TypeOfId Situation)
(TypeOfId Event)
(TypeOfId ContractParam)
(TypeOfId StateVar)
(tns RoleId
	; ns calls
	(absfn envroleid -> **)
	(upcast Id)
)

(macro TupleOfType (Base) ( group
	; NOTE THIS IS JUST SHOWING OFF. DON'T ACTUALLY USE NonemptyBaseTuple, etc.
	(tns BaseTuple
		; ns call
		(absfn empty -> **)
		(absfn singleton (Base) -> NonemptyBaseTuple)
		; dot or ns call
		(absfn prepend (** Base) -> NonemptyBaseTuple)
		(absfn cases (T) (** (EmptyBaseTuple ~> T) (NonemptyBaseTuple ~> T) -> T))
		(absfn match (T) (** ( ~> T) ((Base **) ~> T) -> T))
	)
	(tns EmptyBaseTuple
		; ns calls.
		(absfn mk -> **) ; or use BaseTuple.empty
		; dot or ns call
		(upcast BaseTuple)
	)
	(tns NonemptyBaseTuple
		; ns calls
		(absfn mk (Base) -> **) ; or use BaseTuple.singleton
		(absfn mk (Base BaseTuple) -> **) ; or use BaseTuple.prepend
		; dot or ns call
		(absfn fst (**) -> Base)
		(absfn rest (**) -> BaseTuple)
		(upcast BaseTuple)
	)
))
(TupleOfType Sort) ; Defines SortTuple, EmptySortTuple, NonemptySortTuple
(TupleOfType EventParamId) ; EventParamIdTuple, ...
(TupleOfType EventRule) ; EventRuleTuple, ...

; a better but more verbose name would be ConcatTuple
(macro SeqOfType (Base)
	(tns BaseSeq
		; ns call
		(absfn empty -> **)
		(absfn singleton (Base) -> **)
		; also ns call, but just to test overloading by different arity:
		(absfn mk -> **) ; alias of empty
		(absfn mk (Base) -> **)  ; alias of singleton
		(absfn mk (Base Base) -> **)
		(absfn mk (Base Base Base) -> **)
		; dot or ns call
		(absfn prepend (** Base) -> NonemptyBaseSeq)
		(absfn fst (**) -> Base)
		(absfn rest (**) -> **)
		(absfn cases (T) (** (EmptyBaseSeq ~> T) (NonemptyBaseSeq ~> T) -> T))
		(absfn match (T) (** ( ~> T) ((Base **) ~> T) -> T))
		(absfn concat (** **) -> **)
	)
)
(SeqOfType SEResult) ; defines SEResultSeq
(SeqOfType Statement) ; StatementSeq
(alias Block () StatementSeq)


(tns SortOpApp
	(upcast Sort)
	(absfn mk (SortOpId SortTuple) -> **)
	(absfn id (**) -> SortOpId)
	(absfn args (**) -> SortTuple)
)
(tns AtomicSort
	(upcast Sort)
	(absfn mk (AtomicSortId) -> **)
	(absfn id (**) -> AtomicSortId)
)
(tns Sort
	(absfn cases (T) (** (AtomicSort ~> T) (SortOpApp ~> T)) -> T)
	(absfn match (T) (**
		(AtomicSort (AtomicSortId) 			~> T)
		((SortOpApp (SortOpId SortTuple)) ~> T)
	) -> T)
)

(tns TimeUnit
	(absfn days -> TimeUnit)
	(absfn hours -> TimeUnit)
	(absfn minutes -> TimeUnit)
	(absfn seconds -> TimeUnit)
)

(macro LitOfType (Base)
	(tns BaseLit
		(absfn mk (Base) -> **)
		(absfn lit (**) -> Base)
	)
)
(LitOfType Bool) ; defines BoolLit
(LitOfType Int) ; IntLit
(LitOfType Real) ; RealLit

(tns Literal
	(absfn cases (T) (** (BoolLit ~> T) (IntLit ~> T) (RealLit ~> T)) -> T)
	(absfn match (T) (**
		(Bool ~> T)
		(Int  ~> T)
		(Real ~> T)
	) -> T)
	; this doesn't actually need a different name
	(absfn match2 (T) (**
		((BoolLit Bool) ~> T)
		((IntLit Int)   ~> T)
		((RealLit Real)  ~> T)
	) -> T)
)

(tns Term
	(absfn cases (T) (**
		(ContractParam ~> T)
		(EventParam ~> T)
		(StateVar ~> T)
		(FnApp ~> T)
		(Literal ~> T)) -> T
	)
)
(macro VarOccurrenceType (IdType)
	(tns IdVarOccurrenceType
		(absfn mk (IdType) -> **)
		(absfn id (**) -> IdType)
		(upcast Term)
	)
)
(VarOccurrenceType StateVarId) ; defines StateVarIdOccurrence
(VarOccurrenceType EventParamId)
(VarOccurrenceType ContractParamId)

(macro PairOfType (Left Right)
	(tns PairLeftRight
		(absfn mk (Left Right) -> **)
		(absfn fst (**) -> Left)
		(absfn snd (**) -> Right)
	)
)
; one for each SubstType
(PairOfType StateVarId Term)  ; PairStateVarIdTerm
(PairOfType EventParamId Term)
(PairOfType ContractParamId Term)

(macro SubstType (IdType)
	(tns IdSubstType
		(absfn append (** PairIdTypeTerm) -> **)
	)
)
(SubstType StateVarId) ; defines StateVarIdSubst
(SubstType EventParamId)
(SubstType ContractParamId)

(tns IfElse
	(absfn test (**) -> Term)
	(absfn true_branch (**) -> Block)
	(absfn false_branch (**) -> Block)
	(absfn mk (Term Block Block) -> **)
	(upcast Statement)
)

(tns Assign
	(absfn lhs (**) -> StateVarId)
	(absfn rhs (**) -> Term)
	(upcast Statement)
)

(tns Prove
	(absfn goal (**) -> Term)
	(upcast Statement)
)

(tns Statement
	(absfn cases (T) (**
		(Assign ~> T)
		(IfElse ~> T)
		(Prove ~> T)
	) -> T)
)

(tns Situation
	(absfn id (**) -> SituationId)
	(absfn handlerSet (**) -> EventRuleSet)
 )

(tns Event
	(absfn id (**) -> EventId)
	(absfn transform (**) -> Block)
	(absfn params (**) -> EventParamIdTuple)
	(absfn destid (**) -> SituationId)
)

(tns SEResult
	(absfn info -> **)
	(absfn claimError -> **)
)

(tns Z3Term
  (absfn consistent (**) -> Bool)
  (absfn valid (**) -> Bool)
  (absfn and (** **) -> **)
  (absfn and (** ** **) -> **)
  (absfn and (** ** ** **) -> **)
  (absfn not (**) -> **)
  (absfn implies (** **) -> **)
)

(tns EventRule)
(tns AgentEventRule
	(upcast EventRule)
	(absfn nextevent -> EventId)
	(absfn enabledGuard -> Term)
	(absfn roles -> RoleIdSet)
	(absfn paramConstraint -> Term)
)
(tns DeadlineEventRule
	(upcast EventRule)
	(absfn nextevent -> EventId)
	(absfn enabledGuard -> Term)
	(absfn roles -> RoleIdSet)
	(absfn paramSetters -> EventParamSubst)
)

(macro consistent (x) (Z3Term.consistent x))
(macro z3and (x y) (Z3Term.and x y))
(macro z3not (x) (Z3Term.not x))
(macro z3implies (x y) (Z3Term.implies x y))


(tns SymbExecLSM
	(fn symbexecStmts ((EventParamSubst epsubst) (Event e) (StateVarSubst svsubst) (Term pathform) (Block blck)) -> SEResultSeq
		(blck.match
			( ~> (symbexecSit svsubst P (e.dest)) )
			((fst rest) ~>
				(fst.match
					; Assign
					((lhs rhs) ~> (SymbExecLSM.symbexecStmts (svsubst.add
						; (PairStateVarIdTerm.mk lhs rhs)
						; just for testing a syntax form I haven't used yet:
						(PairStateVarIdTerm.mk lhs ((PairStateVarIdTerm.mk lhs rhs).snd))
					) pathform rest))

				   	; IfElse
				   	((test true_branch false_branch) ~> (
				   		(TESTapplied (subst2 test epsubst svsubst))
				   		(pathform1 = (z3and pathform testapplied))
				   		(rv1 =
				   			(ifelse (consistent pathform1)
				   				(SymbExecLSM.symbexecStmts epsubst e pathform (blck1.concat rest))
				   				(SEResultSeq.empty)
				   			)
				   		)
				   		(pathform2 = (z3and pathform (z3not testapplied)))
				   		(rv2 =
				   			(ifelse (consistent pathform2)
				   				(SymbExecLSM.symbexecStmts epsubst e pathform2 (blck2.concat rest))
				   				(SEResultSeq.empty)
				   			)
				   		)
				   		(Block.concat rv1 rv2)
				   	))

				   	; Prove
				   	(prfoblig ~>
				   		(ifelse (valid (z3implies pathform proofoblig))
							(SymbExecLSM.symbexecStmts epsubst svsubst e pathform rest)
				   			(SEResultSeq.singleton (SEResult.claimError))
				   		)
				   	)
				)
		  )
		)
	)

	(fn symbexecSit ((StateVarSubst svsubst) (Z3Form pathform) (Situation s)) -> SEResultSeq
		(SymbExecLSM.symbexecERules svsubst pathform (s.handlerSet))
	)
	(fn symbexecEvent ((RoleId roleid) (StateVarSubst svsubst) (Z3Term pathform) (Event event)) -> SEResultSeq
		(SymbExecLSM.symbexecERules svsubst event pathform (event.transform))
	)

	(fn symbexecERules ((StateVarSubst svsubst) (Z3Term pathform) (Event event) (EventRuleTuple erules)) -> SEResultSeq
		(erules.match
		  (~> (SEResultSeq.empty))
		  ((rule rest) ~>    (SEResultSeq.concat
		  						(SymbExecLSM.symbexecERule pathform event rule)
		  						(SymbExecLSM.symbexecERules svsubst pathform event rest))
		  )

		)
	)

	(fn symbexecERule ((StateVarSubst svsubst) (Z3Term pathform) (Event event) (EventRule rule) (Nat timeind)) -> SEResultSeq
		(rule.match
			; AgentERule
			((nextevent enabledGuard roles paramConstraint timeConstraint) ~>
				(SEResultSeq.flatmap ( (role) ~> (
					(epsubst = (SymbExecLSM.safeEventParamSEVars nextevent timeind))
					(pathform2 = (z3and
								 (z3and (subst2 epsubst svsubst paramConstraint)
										(subst1 svsubst enabledGuard))
								 		pathform)
					)
					(ifelse (consistent pathform2)
					  	(SymbExecLSM.symbexecEvent role epsubst svsubst pathform2 nextevent)
						(SEResultSeq.empty)
					)
				)) roles)
			)
			; DeadlineERule
			((nextevent enabledGuard paramSetters) ~> (
				(epsubst = (paramSetters.map
					((param term) ~>
						(subst1 svsubst term)
					)
				))
				(pathform2 = (z3and (subst1 svsubst enabledGuard)
							 		pathform)
				)
				(ifelse (consistent pathform2)
				  	(SymbExecLSM.symbexecEvent (RoleId.envroleid) epsubst svsubst pathform2 nextevent)
					(SEResultSeq.empty)
				)
			))
		)
	)
)


; old:
; In Racket
; 1 convert custom S-Expression language to valid Racket, where `macro` uses Racket macros. read file
; 2 use Racket macro expansion
; 3 replace ** and overloads (programmatically, not using a Racket feature)
; 4 normally-coded typecheck and inserting of coercions
; 5 use Racket macro expansion again to expand into Racket, removing unnecessary coercions along the way
; 6	use Racket macro expansion again to expand into some other language

; In TypedRacket
; 1a use type expanders for my macro types https://docs.racket-lang.org/type-expander/index.html?q=typed%20racket#%28part.__lang_and_module_language_combining_type-expander_and_typed_racket_base%29
;

