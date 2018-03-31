# TRACE = True
TRACE = False
from datetime import timedelta
from enum import Enum
import random

from src.model.StateVarDec import StateVarDec
from src.constants_and_defined_types import FULFILLED_SITUATION_LABEL
from src.independent.LedgerDict import LedgerDict, LazyRecTuple
from src.independent.OneUseFrozenDict import OneUseFrozenDict
from src.parse_to_model.sexpr_to_L4Contract import unprimed, isprimed
from src.model.BoundVar import BoundVar
from src.hard_correctness_checks.toSMTLIB import term2smtterm
from src.model.Literal import Literal
from src.independent.util import todo_once
from src.model.Action import Action
from src.model.ActionRule import NextActionRule
from src.model.L4Contract import L4Contract
from src.model.Situation import Situation
from src.model.Statement import Block, Statement, StateVarAssign, IfElse, FVRequirement, LocalVarDec
from src.model.Term import Term, FnApp
from src.model.Sort import Sort as L4Sort
from src.independent.typing_imports import *
from src.hard_correctness_checks.symbolic_execution.z3types_interface import *


# raise Exception("need to distinguish the two kinds of contract 'params' now")

# z3path /Users/dustin/lib/z3-4.6.0-x64-osx-10.11.6/bin/python/z3
# but don't need to be there. just run the following command.
# export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:/Users/dustin/lib/z3-4.6.0-x64-osx-10.11.6/bin; export PYTHONPATH=/Users/dustin/lib/z3-4.6.0-x64-osx-10.11.6/bin/python; python3

class MyTypeError(TypeError):
    def __init__(self, wanted_class:Any, found_object: Any) -> None:
        super(MyTypeError, self).__init__(
            f"Found {found_object}, a {type(found_object)} where only {wanted_class} should be possible.\n"
            "Did you forget to run mypy? Or could be the result of an unsound cast.")

# Intended to be "expanded" (copied with sharing). A minor extension of nested tuples.
Store = LedgerDict[str,Z3Term]
# Intended to be created once, used for reading, and then discarded. A python dict with write ops disabled.
OneUseStore = OneUseFrozenDict[str, Z3Term]

class CoreSymbExecState(NamedTuple):
    # these things are always defined
    path_constraint: Z3Term
    state: Store
    time: int
    env_vars: Store

class GuardedBlockPath(NamedTuple):
    # guard: Z3Term  # already conjoined with path_constraint, for now
    block: Block
    next_state: Store # for Alg2, always frozen. For Alg1, sometimes mutable.
    action: Action
    action_params: OneUseStore
    core: CoreSymbExecState

class ActionRuleEnabledPath(NamedTuple):
    # guard: Z3Term  # already conjoined with path_constraint, for now
    rule: NextActionRule
    # Note that in the ActionRules of a FollowingSituation, action parameters referring to the previous action are
    # forbidden. It's just too confusing. Parser will recommend using a local variable in the StateTransform.
    core: CoreSymbExecState


class ActionRuleParamsConstraintPath(NamedTuple):
    # wherewhen: Z3Term  # already conjoined with path_constraint, for now
    rule: NextActionRule
    action_params: OneUseStore  # newly introduced
    core: CoreSymbExecState


class AssertionPath(NamedTuple):
    assertion: Z3Term
    statement: Statement
    next_state: Store
    action_params: OneUseStore
    core: CoreSymbExecState

QueryPath = Union[GuardedBlockPath, ActionRuleEnabledPath, ActionRuleParamsConstraintPath, AssertionPath]


class SEvalRVChange(NamedTuple):
    # alternatively, could use Z3TermBox
    # but for version 1, let's just use all immutable types. then can see how efficiency improves later!
    next_state: Store
    path_constraint: Z3Term

class SEvalRVBug(NamedTuple):
    msg: str

class SEvalRVInconsistent(NamedTuple):
    msg: str

class SEvalRVStopThread(NamedTuple):
    msg: str

class SEvalRVTimeout(NamedTuple):
    msg: str

SEvalRV = Union[SEvalRVChange, SEvalRVBug, SEvalRVInconsistent, SEvalRVStopThread, SEvalRVTimeout]

pathconstr:Z3Term
state:Store
t:int
envvars:Store

def symbolic_execution(prog:L4Contract):

    contractParams : OneUseStore

    queryPaths : List[QueryPath] = []
    timedoutQueryPaths : List[QueryPath] = []

    path2actionseq : Dict[Any,list] = dict()

    sz3 = z3.Solver()

    def addQueryPath(qpath:QueryPath):
        queryPaths.append(qpath)

    def addTimeoutQueryPath(qpath:QueryPath):
        timedoutQueryPaths.append(qpath)

    def takeQueryPath() -> QueryPath:
        # ind =  random.randint(0,len(queryPaths)-1)
        # qp = queryPaths[ind]
        # del queryPaths[ind]
        qp = queryPaths.pop()
        return qp


    def term2z3(topterm:Union[Term,str],
                next_state:Optional[Store],
                actparam_store:Optional[OneUseStore],
                core: CoreSymbExecState) -> Z3Term:
        pathconstr, state, t, envvar_store = core
        assert isinstance(state, LedgerDict), type(state)
        assert actparam_store is None or isinstance(actparam_store,OneUseFrozenDict)
        assert state is not None

        # print(f"term2z3({topterm}, {state}, {next_state}, {actparam_store})")

        def helper(term:Union[Term,str,int,float,bool]) -> Z3Term:
            assert state is not None
            # print(f"helper({term})", type(term))
            assert term != "immediately" and term != "no_time_constraint", term

            if isinstance(term,str):
                if term in envvar_store:
                    return envvar_store[term]
                elif term in state:
                    assert not isprimed(term), "Seem to have acidentally added a primed var to state."
                    # print(f"in helper({term}) in term2z3({topterm}), looking at state[{term}]:", state[term])
                    return state[term]
                elif next_state and isprimed(term) and unprimed(term) in next_state:
                    assert not term in next_state, "Seem to have acidentally added a primed var to next_state."
                    # print(f"in helper({term}) in term2z3({topterm}), looking at next_state[{unprimed(term)}]:", next_state[unprimed(term)])
                    # print("next_state[halt]:", next_state['halt'])
                    return next_state[unprimed(term)]
                elif actparam_store and term in actparam_store:
                    return actparam_store[term]
                elif term in contractParams:
                    return contractParams[term]
                else:
                    raise Exception(f"Problem at term2z3({term}, {state}, {next_state}, {actparam_store})")
            elif isinstance(term,BoundVar):
                return helper(term.name)
            elif isinstance(term,Literal):
                return helper(term.lit)
            elif isinstance(term, FnApp):
                if term.fnsymb_name in {"trust","check"}:
                    todo_once("not checking `(check Sort Term)` expressions")
                    return helper(term.args[1])
                # smttermstr = str(term2smtterm(term))
                # print("type(term):", type(term))
                # print("smttermstr:", smttermstr)
                #
                # return z3.parse_smt2_string(smttermstr)
                args = [helper(x) for x in term.args]
                if term.fnsymb_name in z3interp:
                    return z3interp[term.fnsymb_name](*args)
                elif term.fnsymb_name in {"last_event_td","next_event_td","last_situation_td"}:
                    return helper(term.fnsymb_name)
                raise NotImplementedError(f"Unhandled {term.fnsymb_name}")
            elif isinstance(term,(bool,int,float)):
                return primValToZ3(term)
            elif isinstance(term,timedelta):
                return timedeltaToZ3(term, prog.timeunit)
            else:
                raise Exception(type(term),term)

        rv : Z3Term = helper(topterm)
        assert rv is not None, topterm
        # print(f"tern2z3({topterm}) == {rv}. type of input ", type(topterm))
        return rv

    def getActionSeq(assignments:LazyRecTuple) -> List[str]:
        seq = dict()
        maxind = 0
        def helper(ll):
            nonlocal maxind
            if ll is None:
                return []
            else:
                pair, rest = ll
                key = pair[0]
                if cast(str,pair[0]).startswith("action"):
                    ind = int(key[7:])
                    seq[ind] = pair[1]
                    maxind = max(maxind,ind)
                helper(rest)
        helper(assignments)
        # return seq
        rv = (maxind+1)*[None]
        for i in range(1,maxind+1):
            rv[i-1] = seq[i]
        return rv



    def sevalAction(a: Action,
                    actparam_store: Optional[OneUseStore], # if Action has params
                    skip_statetransform: bool, # set this to True when calling from `query` after completing a
                                              # state transform evaluation
                    core: CoreSymbExecState
                   ) -> SEvalRV:
        pathconstr, state, t, envvars = core

        if TRACE:
            state = state.set(f"action_{t}", a.action_id)
            # print(f"sevalAction {a.action_id} ; {state} ; {t}; {envvars} ; {skip_statetransform}")
            print("Action seq (reversed):", list(reversed(getActionSeq(state.changes))))

        envvars = envvars.set("last_event_td", envvars["next_event_td"])
        envvars = envvars.set("next_event_td", None)

        assert isinstance(state, LedgerDict), type(state)
        if not skip_statetransform:
            # print(f"{t} {a.action_id}")
            if a.state_transform:
                res = sevalBlock(a.state_transform.statements, Store({}), a, actparam_store,
                                 CoreSymbExecState(pathconstr, state, t, envvars))
                if isinstance(res, SEvalRVChange):
                    assert isinstance(res.next_state, LedgerDict), type(res.next_state)
                    state = res.next_state
                    pathconstr = res.path_constraint
                elif isinstance(res, (SEvalRVInconsistent, SEvalRVBug, SEvalRVTimeout, SEvalRVStopThread)):
                    return res
                else:
                    raise NotImplementedError

        if a.following_anon_situation:
            assert isinstance(state, LedgerDict), type(state)
            return sevalSituation(a.following_anon_situation, CoreSymbExecState(pathconstr,state,t+1,envvars) )
        elif a.dest_situation_id == FULFILLED_SITUATION_LABEL:
            if TRACE:
                print("fulfilled")
            return SEvalRVStopThread("trans to Fulfilled from sevalAction")
        else:
            return sevalSituation(prog.situation(a.dest_situation_id), CoreSymbExecState(pathconstr,state,t+1,envvars) )

    def sevalBlock(block: Block,
                   next_state: Store,
                   a: Action,
                   actparam_store: Optional[OneUseStore],  # if it's in an Action with params
                   core: CoreSymbExecState
                  ) -> SEvalRV:
        pathconstr, state, t, envvars = core
        if TRACE:
            print(f"sevalBlock(...,{state},{next_state}...,{t}")
        assert isinstance(state, LedgerDict), type(state)
        assert isinstance(next_state, LedgerDict), type(next_state)
        if len(block) == 0:
            if state is not next_state:
                for v in state:
                    # print(state)
                    if v not in next_state:
                        next_state = next_state.set(v, state[v])
            return SEvalRVChange(next_state=next_state,path_constraint=pathconstr)
        else:
            rv1 = sevalStatement(block[0], next_state, a, actparam_store, CoreSymbExecState(pathconstr, state, t, envvars ))
            if isinstance(rv1, (SEvalRVInconsistent, SEvalRVBug, SEvalRVTimeout, SEvalRVStopThread)):
                return rv1
            elif isinstance(rv1, SEvalRVChange):
                return sevalBlock(block[1:], rv1.next_state,a, actparam_store, CoreSymbExecState(rv1.path_constraint, state, t, envvars ))
            else:
                raise Exception(rv1)

    def sevalStatement(stmt: Statement,
                       next_state:Store,
                       a:Action,
                       actparam_store: Optional[OneUseStore], # if it's in an Action with params
                       core: CoreSymbExecState
                      ) -> SEvalRV:
        pathconstr, state, t, envvars = core
        if TRACE:
            print(f"sevalStatement({stmt},{state},{next_state},...,{t})")
        if isinstance(stmt, StateVarAssign):
            z3term = term2z3(stmt.value_expr, next_state, actparam_store, core)
            next_state = next_state.set(stmt.varname, z3term)

            return SEvalRVChange(next_state, pathconstr)

        elif isinstance(stmt, IfElse):
            test = term2z3(stmt.test, next_state, actparam_store, core)

            addQueryPath(
                GuardedBlockPath(stmt.true_branch, next_state, a, actparam_store,
                                 CoreSymbExecState(z3and(test, pathconstr), state, t, envvars)) )

            addQueryPath(
                GuardedBlockPath(stmt.false_branch, next_state, a, actparam_store,
                                 CoreSymbExecState(z3and(z3not(test), pathconstr), state, t, envvars)) )

            return SEvalRVStopThread("from sevalStatement")

        elif isinstance(stmt, FVRequirement):
            raise NotImplementedError

            assertion = term2z3(stmt.value_expr, next_state, actparam_store, core)
            # froz_next_state = frozendict(next_state)
            copied_next_state = next_state.copy()
            addQueryPath(
                AssertionPath(assertion, copied_next_state, actparam_store,
                              CoreSymbExecState(pathconstr, state, t, envvars)))

        elif isinstance(stmt, LocalVarDec):
            raise Exception("You need to use eliminate_local_vars before doing symbolic execution")

        else:
            raise MyTypeError(Statement, stmt)

    def sevalSituation(s: Situation,
                       core: CoreSymbExecState
                      ) -> SEvalRV:

        pathconstr, state, t, envvars = core

        # SETTING last_situation_td
        envvars = envvars.set("last_situation_td", envvars["last_event_td"])
        # pathconstr = z3and(envvars["last_situation_td"] == envvars["last_event_td"])

        if TRACE:
            print(f"sevalSituation({s.situation_id},{state},...,{t})")
        assert isinstance(state, LedgerDict), type(state)
        todo_once("QueryPath for each precondition, or their conjunction")

        for rule in s.action_rules():
            assert isinstance(rule,NextActionRule)
            if rule.entrance_enabled_guard:
                enabled_guard_z3 = term2z3(rule.entrance_enabled_guard, None, None,
                                           CoreSymbExecState(pathconstr, state, t, envvars))
                addQueryPath(ActionRuleEnabledPath( rule,
                                                    CoreSymbExecState(z3and(enabled_guard_z3, pathconstr), state, t, envvars)
                ))
            else:
                res = sevalRuleIgnoreGuard(rule, CoreSymbExecState(pathconstr, state, t, envvars))
                if isinstance(res,SEvalRVBug):
                    return res
                elif isinstance(res, (SEvalRVStopThread, SEvalRVInconsistent)):
                    continue
                elif isinstance(res,SEvalRVChange):
                    raise NotImplementedError
                else:
                    raise NotImplementedError

        return SEvalRVStopThread("from sevalSituation")

    def sevalRuleIgnoreGuard(rule:NextActionRule, core:CoreSymbExecState) -> SEvalRV:
        pathconstr, state, t, envvars = core
        assert isinstance(state, LedgerDict)
        action = prog.action(rule.action_id)
        # first need to handle the new action params
        actparam_store: OneUseStore
        if rule.fixed_args:
            d = {actparam_name: rule.fixed_args[
                action.param_name_to_ind[actparam_name]] for actparam_name in action.param_names}
            actparam_store = OneUseStore({actparam_name: term2z3(d[actparam_name], state, None, core) for actparam_name in d})
        else:
            actparam_store = OneUseStore({actparam_name:
                name2actparam_symbolic_var(actparam_name,t,action.param_sorts_by_name[actparam_name],sz3) for actparam_name in action.param_names})

        # SETTING next_event_td
        envvars = envvars.set("next_event_td", name2symbolicvar(f"td_{t}",'TimeDelta',sz3))
        if t > 0:
            pathconstr = z3and(name2symbolicvar(f"td_{t-1}",'TimeDelta') <= name2symbolicvar(f"td_{t}",'TimeDelta'), pathconstr)

        if rule.time_constraint is None and rule.where_clause is None:
            return sevalRuleIgnoreGuardAndParamConstraints(rule, actparam_store, CoreSymbExecState(pathconstr, state, t, envvars))
        else:
            newcore = CoreSymbExecState(pathconstr, state, t, envvars)
            tc : Z3Term = term2z3(rule.time_constraint, None, actparam_store, newcore) \
                if (rule.time_constraint and rule.time_constraint != "no_time_constraint") else  Z3TRUE
            wc : Z3Term = term2z3(rule.where_clause, None, actparam_store, newcore) if rule.where_clause else Z3TRUE
            addQueryPath( ActionRuleParamsConstraintPath(
                rule, actparam_store, CoreSymbExecState( z3and(tc, wc, pathconstr), state, t, envvars)
            ))
            return SEvalRVStopThread("from sevalRuleIgnoreGuard")


    def sevalRuleIgnoreGuardAndParamConstraints(
                             rule:NextActionRule,
                             actparam_store: OneUseStore,
                             core: CoreSymbExecState) -> SEvalRV:
        if TRACE:
            print(f"sevalRuleIgnoreGuardAndParamConstraints({rule} ; {core.state})")
        assert isinstance(core.state, LedgerDict)
        action = prog.action(rule.action_id)
        return sevalAction(action, actparam_store, False, core)

    def check(formula:z3.ExprRef) -> z3.CheckSatResult:
        sz3.push()
        sz3.add(formula)
        rv = sz3.check()
        if rv == z3.unknown:
            print("Reason unknown:", sz3.reason_unknown())
        sz3.pop()
        return rv

    def query(qp:QueryPath) -> SEvalRV:
        if isinstance(qp, GuardedBlockPath):
            qp = cast(GuardedBlockPath,qp)
            checkres = check(qp.core.path_constraint)
            # print(f"check({qp.core.path_constraint})\nis {checkres}")
            if checkres == z3.sat:
                res = sevalBlock(qp.block, qp.next_state, qp.action, qp.action_params, qp.core)
                if isinstance(res, SEvalRVChange):
                    # the following is only sound under the constraint that IfElse Statements only occur as the last
                    # Statement in a Block.
                    return sevalAction(qp.action, qp.action_params, True,
                                       CoreSymbExecState(res.path_constraint, res.next_state, qp.core.time, qp.core.env_vars))

                if isinstance(res, SEvalRVBug):
                    return res

            elif checkres == z3.unsat:
                return SEvalRVInconsistent("GuardedBlockPath query unsat")

        elif isinstance(qp, ActionRuleEnabledPath):
            checkres = check(qp.core.path_constraint)
            if checkres == z3.sat:
                sevalRuleIgnoreGuard(qp.rule, qp.core)
            elif checkres == z3.unsat:
                return SEvalRVInconsistent("ActionRuleEnabledPath query unsat")

        elif isinstance(qp, ActionRuleParamsConstraintPath):
            assert isinstance(qp.core.state, LedgerDict)
            # print("z3 query", qp.core.path_constraint)
            checkres = check(qp.core.path_constraint)
            # invalid
            # syntax( < string >, line
            # 1)
            if checkres == z3.sat:
                if TRACE:
                    print("sat:", z3termPrettyPrint(qp.core.path_constraint))
                return sevalRuleIgnoreGuardAndParamConstraints(qp.rule, qp.action_params, qp.core)
            elif checkres == z3.unsat:
                if TRACE:
                    print("unsat:", qp.core.path_constraint)
                return SEvalRVInconsistent("ActionRuleParamsConstraintPath query unsat")

        elif isinstance(qp, AssertionPath):
            checkres = check(z3.And(z3.Not(qp.assertion), qp.core.path_constraint))
            raise NotImplementedError("know assertions till working without them")

        else:
            raise NotImplementedError(qp)

        if checkres == z3.unknown:
            if TRACE:
                print(f"UNKNOWN: {qp}")
            addTimeoutQueryPath(qp)
            simplified = z3.simplify(qp.core.path_constraint)
            print("Theorem prover gave up on this (now simplified) query:\n\n", simplified)
            # sz3.push()
            # sz3.add(simplified)
            # print("Try again:",sz3.check(simplified), sz3.reason_unknown())
            # sz3.pop()

            return SEvalRVTimeout(str(qp.core.path_constraint))
            # raise NotImplementedError("don't want to be dealing with timeouts till working with easy examples")

    def pathChooser():

        while len(queryPaths) > 0:
            qpath = takeQueryPath()
            # print("paths: ", len(queryPaths), qpath)
            print("#paths: ", len(queryPaths), len(timedoutQueryPaths))
            # this call will add paths to queryPaths:
            res = query(qpath)
            if isinstance(res, SEvalRVTimeout):
                addTimeoutQueryPath(qpath)
            elif isinstance(res, SEvalRVBug):
                raise Exception(res.msg)
            # we don't need to do anything in the other two SEvalRV cases?

    for_contractParams : Dict[str,str] = dict()
    for paramname,dec in prog.contract_params.items():
        for_contractParams[paramname] = name2initstateval_symbolic_var(paramname, dec.sort, sz3)
    contractParams = OneUseFrozenDict[str,str](for_contractParams)

    envvars: Store = Store({
        # "last_event_td" : 'td_0',
        # "last_situation_td": 'td_0'
        "last_event_td": name2symbolicvar('td_0',"TimeDelta",sz3),
        "last_situation_td": name2symbolicvar('td_0',"TimeDelta")
    })
    pathconstr = (name2symbolicvar('td_0','TimeDelta') == 0)

    startstate : Store = Store({})
    dec : StateVarDec
    for dec in prog.state_var_decs.values():
        if dec.initval:
            startstate = startstate.set(dec.name, term2z3(dec.initval, None, None, CoreSymbExecState(pathconstr, startstate, 0, envvars)))
    startcore = CoreSymbExecState(pathconstr, startstate, 0, envvars)

    sevalSituation(prog.situation(prog.start_situation_id), startcore)
    pathChooser()

todo_once("Keep time and other constraints separate, for debugging")