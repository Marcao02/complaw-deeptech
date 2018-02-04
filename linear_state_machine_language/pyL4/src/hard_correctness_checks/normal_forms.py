from typing import FrozenSet

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

    for act in p.actions_iter():
        if act.global_state_transform:
            (x,y,z) = eliminate_local_vars_block(act.global_state_transform.statements, {}, set(), set())
            act.global_state_transform.statements = x

"""
This both modifies its arguments and returns something, which is naughty.
Modify `prog`, eliminating every instance of local var usage.

"""
# def eliminate_local_vars_block(block: Block, subst:Dict[str,Term], forbidden_read:Set[str], forbidden_write:Set[str]) -> Tuple[Block,FrozenSet[str],FrozenSet[str]]:
def eliminate_local_vars_block(block: Block, subst: Dict[str, Term], forbidden_read: Set[str],
                               forbidden_write: Set[str]) -> Tuple[Block, Set[str], Set[str]]:
    assert block is not None
    if len(block) == 0:
        return ([], forbidden_read, forbidden_write)

    rest = block[1:]
    s = block[0]

    if isinstance(s, LocalVarDec):
        assert s.varname not in forbidden_write, f"local var {s.varname} can't be written at (TODO: s.coord)"
        value_expr2 = eliminate_local_vars_term(s.value_expr, frozendict(subst), frozenset(forbidden_read))
        subst2 = subst.copy()
        subst2[s.varname] = value_expr2

        # can't write a second time to this variable in the same forward-scope, because it's potentially confusing
        # and not needed in good code.
        forbidden_write2 = forbidden_write.union({s.varname})

        # Now that the term giving s.varname its new value has been substituted, we move on to eliminating further
        # local var decs in the remainder of the block.
        # return eliminate_local_vars_block(rest, subst, forbidden_read, forbidden_write2)
        # EXCEPT that there's a subtley with eliminating them in action rules, so I won't actually eliminate them,
        # but I will ignore them when doing FV confined to the state transform section
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

        # forbid reading from it in the forward-scope after writing to it, because that's potentially confusing
        # and not needed in good code.
        # ACTUALLY I rely on this for translation in action rules to be sound.
        forbidden_read.add(s.varname)

        s.value_expr = eliminate_local_vars_term(s.value_expr, frozendict(subst), frozenset(forbidden_read))

        (new_rest, new_forbidden_read, new_forbidden_write) = eliminate_local_vars_block(rest, subst,
                                                                                         forbidden_read,
                                                                                         forbidden_write)
        return cast(Block,[s]) + new_rest, new_forbidden_read, new_forbidden_write

    elif isinstance(s, IfElse):
        newtest = eliminate_local_vars_term(s.test, frozendict(subst), frozenset(forbidden_read))

        # copy_read_forbid = forbidden_read.copy()
        # copy_write_forbid = forbidden_write.copy()
        (new_true_branch, forbidden_read1, forbidden_write1) = eliminate_local_vars_block(s.true_branch, subst,
                                                                                          forbidden_read,
                                                                                          forbidden_write)
        # global vars written to in the true branch can be written to in the false branch, so we send the same sets
        (new_false_branch, forbidden_read2, forbidden_write2) = eliminate_local_vars_block(s.false_branch, subst,
                                                                                           forbidden_read,
                                                                                           forbidden_write) if s.false_branch is not None else ([],set(),set())

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


def eliminate_local_vars_term(term: Term, subst:Dict[str,Term],  forbidden_read:FrozenSet[str]) -> Term:
    if isinstance(term, LocalVar):
        assert term.name not in forbidden_read, f"local var {term} can't be read at {term.coord}"
        if term.name in subst:
            print("SUBST!", term.name, subst[term.name])
            return subst[term.name]
    elif isinstance(term, GlobalVar):
        assert term.name not in forbidden_read, f"global-state var {term} can't be read at {term.coord}"
    elif isinstance(term, FnApp):
        return FnApp(term.fnsymb_name, [eliminate_local_vars_term(arg, subst, forbidden_read) for arg in term.args], term.coord)
    return term


SORT_TO_PRED = {
    'Pos' : lambda x: x > 0,
    'Nonneg' : lambda x: x >= 0,
}

def gen_cast_proof_obligs(p:L4Contract):

    def okblock(block: Block) -> OutType:
        assert block is not None and len(block) > 0
        s = block[0]
        rest = block[1:]
        if isinstance(s,StateVarAssign):
            e = okterm(s.value_expr)
            rest2 = [s2.subst(s.varname, s.value_expr) for s2 in rest]
            return assert_assume(e, okblock(rest2))
        elif isinstance(s,LocalVarDec):
            return okblock(rest)
        elif isinstance(s,IfElse):
            test2 = okterm(s.test)
            trueblock = okblock(s.true_branch)
            falseblock = okblock(s.false_branch) if s.false_branch else None
            branch_correctness : OutType
            if falseblock is not None:
                branch_correctness = conj(
                    implies(test2, trueblock),
                    implies(neg(test2), falseblock)
                )
            else:
                branch_correctness = implies(test2, trueblock)
            return assert_assume(branch_correctness, okblock(rest))
        elif isinstance(s,FVRequirement):
            e = okterm(s.value_expr)
            return assert_assume(e, okblock(rest))
        else:
            raise NotImplemented

    def okterm(t: Term) -> OutType:
        requirements : Any
        if isinstance(t,FnApp):
            if t.fnsymb_name == "cast":
                pass
        raise NotImplemented




#
#
#     """
#     Q1: Should this be allowed?
#     if test
#         local x = 1
#     else
#         local x = 2
#     remainder_of_block
#     I do use it in that way in TransferCash_L and TransferCash_D
#     BUT I could easily rewrite using function ifthenelse
#     (liq_cashout =
#         (ifthenelse (cash_currently_unconverted ≤ (company_cash * investor_percent_of_cashout_investor_investments))
# 		    cash_currently_unconverted
# 			(company_cash * investor_percent_of_cashout_investor_investments)
# 		)
# 	)
# 	I should keep lexical scoping and freedom from side effects unless there's a good reason not to.
# 	So my answer is NO.
#
# 	Q2:
# 	Conditions:
# 	You can't reassign local variables. Use a new one. Try adding a ' or a 2 or a _.
#
# 	You can't write to a state variable twice in any execution.
# 	Instead of
# 	    $x = t1
# 	    if blah:
# 	        $x = t2
# 	You'd need to write
# 	    $x = if blah then t2 else t1
# 	or
# 	    if blah
# 	        $x = t2
# 	    else
# 	        $x = t1
# 	Instead of
# 	    $x = t1
# 	    if blah:
# 	        local y = t3
# 	        $x = f(y,y)
# 	You'd need to write
# 	    if blah
# 	        local y = t3
# 	        $x = f(y,y)
# 	    else
# 	        $x = t1
#
# 	You can't read from a state variable after you write to it.
# 	Instead of:
# 	    $x = t
# 	    $y = f($x)
# 	You must write:
# 	    new_x = t
# 	    $x = new_x
# 	    $y = f(new_x)
# 	Alternatively, we could use the primed variables convention for the-next-value.
#     """