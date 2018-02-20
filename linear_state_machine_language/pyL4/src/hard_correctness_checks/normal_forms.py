from typing import FrozenSet

from src.constants_and_defined_types import LocalVarId
from src.independent.util import chcast
from src.model.Term import Term, FnApp
from src.model.BoundVar import LocalVar, GlobalVar
from src.independent.typing_imports import *
from src.model.Statement import LocalVarDec, StateVarAssign, IfElse, FVRequirement, Statement
from src.model.L4Contract import L4Contract

Block = List[Statement]
OutType = Union[str,Tuple[Any,...]]

def disj(*args:OutType) -> OutType:
    return ("or",) + args # type:ignore
def conj(*args:OutType) -> OutType:
    return ("and",) + args # type:ignore
def neg(arg:OutType) -> OutType:
    return ("not", arg) # type:ignore
def implies(arg1:OutType, arg2:OutType) -> OutType:
    return ("or",("not",arg1), arg2) # type:ignore
def fnapp(symb:str,*args:OutType) -> OutType:
    return (symb,) + args # type:ignore

def assert_assume(e1:OutType,e2:OutType) -> OutType:
    return conj(e1,implies(e1,e2))

# place holder for if we introduce an immutable dictionary type later
FrozenDict = Dict
def frozendict(d:Dict[T1,T2]) -> FrozenDict[T1,T2]:
    return d

def eliminate_ifthenelse(p:L4Contract):
    progress_made = False

    def isite(t:Term) -> bool:
        return isinstance(t,FnApp) and t.fnsymb_name == "ifthenelse"

    def eliminate_ifthenelse_block(block:Block) -> Block:
        global progress_made
        if len(block) == 0:
            return block
        rest = block[1:]
        s = block[0]

        maybe_ite_term = s.findFirstTerm(isite)
        snew : Statement
        if not maybe_ite_term:
            snew = s
        else:
            progress_made = True # type:ignore
            ite_term = chcast(FnApp,maybe_ite_term)
            assert ite_term.fnsymb_name == "ifthenelse", ite_term
            assert len(ite_term.args) == 3, ite_term.args
            ite_test, ite_true_expr, ite_false_expr = ite_term.args
            if isinstance(s, LocalVarDec):
                raise Exception("Use eliminate_local_vars first")

            elif isinstance(s, StateVarAssign):
                snew = IfElse(ite_test,
                              [StateVarAssign(s.vardec,s.value_expr.substForTerm(ite_term, ite_true_expr), s.varop)],
                              [StateVarAssign(s.vardec, s.value_expr.substForTerm(ite_term, ite_false_expr), s.varop)]
                              )
                return cast(Block, [snew]) + eliminate_ifthenelse_block(rest)

            elif isinstance(s, IfElse):
                maybe_occurence = s.test.findFirstTerm(lambda y: y == ite_term)
                if not maybe_occurence:
                    if s.false_branch is not None:
                        snew = IfElse(s.test,
                                      eliminate_ifthenelse_block(s.true_branch),
                                      eliminate_ifthenelse_block(s.false_branch))
                    else:
                        snew = IfElse(s.test,
                                      eliminate_ifthenelse_block(s.true_branch))
                else:
                    snew = IfElse(ite_test,
                                  [s.substForTerm(ite_test, ite_true_expr)],
                                  [s.substForTerm(ite_test, ite_false_expr)])

            elif isinstance(s, FVRequirement):
                maybe_occurence = s.value_expr.findFirstTerm(lambda y: y == ite_term)
                if not maybe_occurence:
                    snew = s
                else:
                    snew = IfElse(ite_test,
                                  [s.substForTerm(ite_test, ite_true_expr)],
                                  [s.substForTerm(ite_test, ite_false_expr)])

            else:
                raise NotImplementedError

        return cast(Block, [snew]) + eliminate_ifthenelse_block(rest)

    def eliminate_ifthenelse_term(term:Term) -> Term:
        if isinstance(term, FnApp):
            return FnApp(term.fnsymb_name, [eliminate_ifthenelse_term(x) for x in term.args], term.coord)
        else:
            return term

    for act in p.actions_iter():
        if act.global_state_transform:
            # but how do I know a change has been made..? can't rely on immutability since Statement and Term aren't immutable.
            progress_made = True
            while progress_made:
                progress_made = False
                act.global_state_transform.statements = eliminate_ifthenelse_block(act.global_state_transform.statements)

        if act.following_anon_situation:
            sit = act.following_anon_situation
            for rule in sit.action_rules():
                # print(rule, rule.fixed_args)
                if rule.fixed_args:
                    for i in range(len(rule.fixed_args)):
                        if rule.fixed_args[i].findFirstTerm(lambda x:isinstance(x,FnApp) and x.fnsymb_name=="ifthenelse"):
                            raise NotImplementedError("Elimination of ifthenelse expression in action rules is not yet implemented (but nothing preventing it from being).")
                if rule.where_clause:
                    if rule.where_clause.findFirstTerm(lambda x:isinstance(x,FnApp) and x.fnsymb_name=="ifthenelse"):
                        raise NotImplementedError(
                            "Elimination of ifthenelse expression in action rules is not yet implemented (but nothing preventing it from being).")

# class EliminateLocalVarsInBlockResult(NamedTuple):
#     block: Block
#     forbidden_read: Set[str]
#     forbidden_write: Set[str]
#     allsubst: Dict[LocalVarId,Term]


def eliminate_local_vars(p:L4Contract):
    print("WARNING I PLAYED FAST AND LOOSE WITH MUTABLE DATA STRUCTURES")
    """Currently unchecked conditions: 
	You can't reassign local variables. Use a new one. Try adding a ' or a 2 or a _.
	
	You can't write to a state variable twice in any execution. 
	Instead of
	    $x = t1
	    if blah:
	        $x = t2
	You'd need to write
	    $x = if blah then t2 else t1
	or
	    if blah
	        $x = t2
	    else
	        $x = t1 
	Instead of
	    $x = t1
	    if blah:
	        local y = t3
	        $x = f(y,y)
	You'd need to write
	    if blah
	        local y = t3
	        $x = f(y,y)
	    else
	        $x = t1
	
	You can't read from a state variable after you write to it. 
	Instead of: 
	    $x = t
	    $y = f($x)
	You must write:
	    new_x = t
	    $x = new_x
	    $y = f(new_x)
	Alternatively, we could use the primed variables convention for the-next-value."""

    """
    This both modifies its arguments and returns something, which is naughty.
    Modify `prog`, eliminating every instance of local var usage.

    """

    allsubst : Dict[str,Term] = dict()

    # def eliminate_local_vars_block(block: Block, substForVar:Dict[str,Term], forbidden_read:Set[str], forbidden_write:Set[str]) -> Tuple[Block,FrozenSet[str],FrozenSet[str]]:
    def eliminate_local_vars_block(block: Block,
                                   subst: Dict[str, Term],
                                   forbidden_read: Set[str],
                                   forbidden_write: Set[str]) \
            -> Tuple[Block, Set[str], Set[str]]:
            # -> EliminateLocalVarsInBlockResult:
        assert block is not None
        if len(block) == 0:
            return ([], forbidden_read, forbidden_write)

        rest = block[1:]
        s = block[0]

        if isinstance(s, LocalVarDec):
            # print(f"seeing local var {s.varname}")
            assert s.varname not in forbidden_write, f"local var {s.varname} can't be written at (TODO: s.coord)"
            value_expr2 = eliminate_local_vars_term(s.value_expr, frozendict(subst), frozenset(forbidden_read))
            subst2 = subst.copy()
            subst2[s.varname] = value_expr2
            allsubst[s.varname] = value_expr2

            # can't write a second time to this variable in the same forward-scope, because it's potentially confusing
            # and not needed in good code.
            forbidden_write2 = forbidden_write.union({s.varname})
            # print(f"just added local var name {s.varname} to forbidden_write after recursing on RHS of {s}")

            # Now that the term giving s.varname its new value has been substituted, we move on to eliminating further
            # local var decs in the remainder of the block.
            # return eliminate_local_vars_block(rest, substForVar, forbidden_read, forbidden_write2)
            # EXCEPT that there's a subtley with eliminating them in action rules, so I won't actually eliminate them,
            # but I will ignore them when doing FV confined to the state transform situation
            # ACTUALLY, it's all good because no-read-after-write condition ensures that local variable definitions only
            # depend on current global var vals, not next global var vals
            # (new_rest, new_forbidden_read, new_forbidden_write) = eliminate_local_vars_block(rest, subst2, forbidden_read, forbidden_write2)
            # return cast(Block, [s]) + new_rest, new_forbidden_read, new_forbidden_write
            return eliminate_local_vars_block(rest, subst2, forbidden_read, forbidden_write2)


        elif isinstance(s, StateVarAssign):
            assert s.varname not in forbidden_write, f"global-state var {s.varname} can't be written at (TODO: s.coord)"
            # forbid writing a second time to this variable in the same forward-scope, because it's potentially confusing
            # and not needed in good code.
            # I was actually doing this... but could easily switch to ifthenelse
            forbidden_write.add(s.varname)

            s.value_expr = eliminate_local_vars_term(s.value_expr, frozendict(subst), frozenset(forbidden_read))

            # forbid reading from it in the forward-scope after writing to it, because that's potentially confusing
            # and not needed in good code.
            # ACTUALLY I rely on this for translation in action rules to be sound.
            # forbidden_read.add(s.varname)
            # print(f"just added global var name {s.varname} to forbidden_read after recursing on RHS of {s}")

            (new_rest, new_forbidden_read, new_forbidden_write) = eliminate_local_vars_block(rest, subst,
                                                                                             forbidden_read,
                                                                                             forbidden_write)
            # new_forbidden_read.discard(s.varname)

            return cast(Block, [s]) + new_rest, new_forbidden_read, new_forbidden_write

        elif isinstance(s, IfElse):
            newtest = eliminate_local_vars_term(s.test, frozendict(subst), frozenset(forbidden_read))

            copy_forbidden_read = forbidden_read.copy()
            copy_forbidden_write = forbidden_write.copy()
            (new_true_branch, forbidden_read1, forbidden_write1) = eliminate_local_vars_block(s.true_branch, subst,
                                                                                              forbidden_read,
                                                                                              forbidden_write)
            # global vars written to in the true branch can be written to in the false branch, so we send the same sets
            (new_false_branch, forbidden_read2, forbidden_write2) = eliminate_local_vars_block(s.false_branch, subst,
                                                                                               copy_forbidden_read,
                                                                                               copy_forbidden_write) if s.false_branch is not None else (
            [], set(), set())

            s.test = newtest
            s.true_branch = new_true_branch
            s.false_branch = new_false_branch

            (new_rest, new_forbidden_read, new_forbidden_write) = \
                eliminate_local_vars_block(rest, subst,
                                           forbidden_read1.union(forbidden_read2),
                                           forbidden_write1.union(forbidden_write2))
            return cast(Block, [s]) + new_rest, new_forbidden_read, new_forbidden_write

        elif isinstance(s, FVRequirement):
            s.value_expr = eliminate_local_vars_term(s.value_expr, frozendict(subst), frozenset(forbidden_read))

            (new_rest, new_forbidden_read, new_forbidden_write) = eliminate_local_vars_block(rest, subst,
                                                                                             forbidden_read,
                                                                                             forbidden_write)
            return cast(Block, [s]) + new_rest, new_forbidden_read, new_forbidden_write

        else:
            raise NotImplementedError

    def eliminate_local_vars_term(term: Term, subst: Dict[str, Term], forbidden_read: FrozenSet[str]) -> Term:
        if isinstance(term, LocalVar):
            assert term.name not in forbidden_read, f"local var {term} can't be read at {term.coord}"
            if term.name in subst:
                # print("SUBST!", term.name, substForVar[term.name])
                return subst[term.name]
        elif isinstance(term, GlobalVar):
            pass
            # assert term.name not in forbidden_read, f"global-state var {term} can't be read at {term.coord}. {forbidden_read}"
        elif isinstance(term, FnApp):
            # assert term.coord is not None, f"No FileCoord for term {term}"
            return FnApp(term.fnsymb_name, [eliminate_local_vars_term(arg, subst, forbidden_read) for arg in term.args],
                         term.coord)
        return term

    for act in p.actions_iter():
        if act.global_state_transform:
            allsubst = dict()
            (x,y,z) = eliminate_local_vars_block(act.global_state_transform.statements, {}, set(), set())
            act.global_state_transform.statements = x
        if act.following_anon_situation:
            sit = act.following_anon_situation
            for rule in sit.action_rules():
                # print(rule, rule.fixed_args)
                if rule.fixed_args:
                    for i in range(len(rule.fixed_args)):
                        rule.fixed_args[i] = eliminate_local_vars_term(rule.fixed_args[i], allsubst, frozenset())
                if rule.where_clause:
                    rule.where_clause = eliminate_local_vars_term(rule.where_clause, allsubst, frozenset())

    # for sit in p.situations_iter():
                    # rule.arg_vars_bound_by_rule[i]))
                    # rule.args[i] =

