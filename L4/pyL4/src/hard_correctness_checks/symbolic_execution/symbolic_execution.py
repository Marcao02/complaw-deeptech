from datetime import datetime

from src.independent.PushOnlyStack import PushOnlyStack
from src.independent.util_for_tuple_linked_lists import TupleLinkedList
from src.model.ContractParamDec import ContractParamDec
from src.independent.util import chcast

# TRACE = True
TRACE = False
USE_CONTRACT_PARAM_VALS = True
# USE_CONTRACT_PARAM_VALS = False

from src.model.StateVarDec import StateVarDec
from src.constants_and_defined_types import FULFILLED_SITUATION_LABEL
from src.independent.LedgerDict import LedgerDict, LazyRecTuple
from src.independent.OneUseFrozenDict import OneUseFrozenDict
from src.parse_to_model.sexpr_to_L4Contract import unprimed, isprimed
from src.model.BoundVar import BoundVar
from src.model.Literal import Literal, SortLit, EventIdLit
from src.model.Action import Action
from src.model.EventRule import EventRule
from src.model.L4Contract import L4Contract
from src.model.Situation import Situation
from src.model.Statement import StatementList, Statement, StateVarAssign, IfElse, FVRequirement, LocalVarDec
from src.model.Term import Term, FnApp
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
    time_path_constraint: Z3Term
    state: Store
    time: int
    env_vars: Store
    extra: PushOnlyStack[Any]
    # type_constraint: Z3Term # avoid this by the constraint that any name in the program has a unique type
    def full_constraint(self) -> Z3Term:
        return conj(self.path_constraint, self.time_path_constraint)


class GuardedBlockPath(NamedTuple):
    # guard: Z3Term  # already conjoined with path_constraint, for now
    block: StatementList
    next_state: Store # for Alg2, always frozen. For Alg1, sometimes mutable.
    next_statement: Optional[Statement]
    action: Action
    action_params: Optional[OneUseStore]
    core: CoreSymbExecState

    # differs from default __str__ just by not calling the action's __str__
    def __str__(self) -> str:
        return f"GuardedBlockPath(block={self.block}, next_state={self.next_state}, next_statement={self.next_statement}, action={self.action.action_id}, action_params={self.action_params}, core={self.core}"

class EventRuleEnabledPath(NamedTuple):
    # guard: Z3Term  # already conjoined with path_constraint, for now
    rule: EventRule
    # Note that in the EventRules of a FollowingSituation, action parameters referring to the previous action are
    # forbidden. It's just too confusing. Parser will recommend using a local variable in the StateTransform.
    core: CoreSymbExecState


class EventRuleParamsConstraintPath(NamedTuple):
    # wherewhen: Z3Term  # already conjoined with path_constraint, for now
    rule: EventRule
    action_params: OneUseStore  # newly introduced
    core: CoreSymbExecState


class AssertionPath(NamedTuple):
    assertion: Z3Term
    next_statement: Optional[Statement] # if this is an in-code assertion, as opposed to one we create on the fly, we need to know where to go next.
    next_state: Store
    action: Action
    action_params: Optional[OneUseStore]
    core: CoreSymbExecState

QueryPath = Union[GuardedBlockPath, EventRuleEnabledPath, EventRuleParamsConstraintPath, AssertionPath]


class SEvalRVChange(NamedTuple):
    # alternatively, could use Z3TermBox
    # but for version 1, let's just use all immutable types. then can see how efficiency improves later!
    next_state: Store
    path_constraint: Z3Term
    extra: PushOnlyStack[Any]

# class SEvalRVBug(NamedTuple):
#     msg: str

class SEvalRVInconsistent(NamedTuple):
    msg: str

class SEvalRVStopThread(NamedTuple):
    msg: str

SEvalRV = Union[SEvalRVChange, SEvalRVInconsistent, SEvalRVStopThread]

pathconstr:Z3Term
state:Store
t:int
envvars:Store


min_writes : Dict[str,int] = dict()
max_writes : Dict[str,int] = dict()

def symbolic_execution(prog:L4Contract):

    fulfilled_cnt = 0

    contractParams : OneUseStore

    queryPaths : List[QueryPath] = []
    timedoutQueryPaths : List[QueryPath] = []
    unprovedAssertions: List[AssertionPath] = []

    sz3 = Z3.Solver()
    for opt,val in OPTIONS.items():
        sz3.set(opt,val)

    def addQueryPath(qpath:QueryPath):
        if TRACE:
            print("Adding query path:", qpath)
        queryPaths.append(qpath)

    def addTimeoutQueryPath(qpath:QueryPath):
        timedoutQueryPaths.append(qpath)

    def addUnprovedAssertion(qpath:AssertionPath):
        unprovedAssertions.append(qpath)

    def takeQueryPath() -> QueryPath:
        # ind =  random.randint(0,len(queryPaths)-1)
        # qp = queryPaths[ind]
        # del queryPaths[ind]
        qp = queryPaths.pop()
        return qp


    def term2z3(topterm:Union[Term,str],
                next_state:Optional[Store],
                actparam_store:Optional[OneUseStore],
                core: CoreSymbExecState,
                propsubst: Optional[Dict[str,bool]] = None) -> Z3Term:
        pathconstr, time_pathconstr, state, t, envvars, extra = core
        assert isinstance(state, LedgerDict), type(state)
        assert actparam_store is None or isinstance(actparam_store,OneUseFrozenDict)
        assert state is not None

        # print(f"term2z3({topterm}, {state}, {next_state}, {actparam_store})")

        def helper(term:Union[Term,str,int,float,bool]) -> Z3Term:
            assert state is not None
            # print(f"helper({term})", type(term))
            assert term != "immediately" and term != "no_time_constraint", term

            if isinstance(term,str):
                if term in envvars:
                    return envvars[term]
                elif term in state:
                    assert not isprimed(term), "Seem to have acidentally added a primed var to state."
                    # print(f"in helper({term}) in term2z3({topterm}), looking at state[{term}]:", state[term])
                    return state[term]

                elif next_state and isprimed(term):
                    assert not term in next_state, "Seem to have acidentally added a primed var to next_state."
                    # print(f"in helper({term}) in term2z3({topterm}), looking at next_state[{unprimed(term)}]:", next_state[unprimed(term)])
                    # print("next_state[halt]:", next_state['halt'])
                    assert unprimed(term) in next_state, str(next_state) + "\n" + str(topterm)
                    return next_state[unprimed(term)]
                elif actparam_store and term in actparam_store:
                    return actparam_store[term]
                elif term in contractParams:
                    return contractParams[term]
                elif propsubst and term in propsubst:
                    return Z3.BoolVal(propsubst[term])
                else:
                    raise Exception(f"Don't recognize term {term} in" "\n" f"helper({term}) in term2z3({topterm}, {state}, {next_state}, {actparam_store}). See {topterm.coord if isinstance(topterm,Term) else '?'}")
            elif isinstance(term,BoundVar):
                return helper(term.name)
            elif isinstance(term, EventIdLit):
                return cast(Any,term.lit)
            elif isinstance(term,Literal):
                return helper(term.lit)
            elif isinstance(term, FnApp):
                if term.fnsymb_name in {"trust","check","units"}:
                    todo_once("NOTE assuming all types are correct, even those using 'trust' or 'check', because"
                              " at the moment symbolic exec is failing to prove them, but previous use of SMT worked.")
                              # " so, not checking `(check Sort Term)` expressions")
                    # sortlit = chcast(SortLit,term.args[0])
                    # addQueryPath( AssertionPath(
                    #     SORT_TO_PRED[cast(str,sortlit.lit)](helper(term.args[1])),
                    #     None, None, core
                    # ))
                    # for now, just skipping the sort argument
                    return helper(term.args[1])
                # smttermstr = str(term2smtterm(term))
                # print("type(term):", type(term))
                # print("smttermstr:", smttermstr)
                #
                # return Z3.parse_smt2_string(smttermstr)
                args = [helper(x) for x in term.args]
                if term.fnsymb_name in z3interp:
                    try:
                        return z3interp[term.fnsymb_name](*args) # type:ignore
                    except Exception as e:
                        print(f"problem evaluating {term} at {term.coord} ({prog.filename}). specifically {term.fnsymb_name} on {args}")
                        print(f"envvars: ", envvars)
                        print(f"state: ", state)
                        raise e

                elif term.fnsymb_name in {"last_event_td","next_event_td","last_situation_td","next_event_dt","last_event_name"}:
                    return helper(term.fnsymb_name)

                elif term.fnsymb_name == "dt2td":
                    raise Exception("dt2td deprecated in favour of always converting DateTimes to TimeDeltas")
                    # return chcast(args[0]) - name2symbolicvar("td_0",'TimeDelta')

                raise NotImplementedError(f"Unhandled {term.fnsymb_name}")
            elif isinstance(term,(bool,int,float)):
                return primValToZ3(term)
            elif isinstance(term,timedelta):
                return timedeltaToZ3(term, prog.timeunit)
            elif isinstance(term,datetime):
                return timedeltaToZ3(term - name2symbolicvar("dt_0","DateTime",sz3), prog.timeunit)
            else:
                raise Exception(type(term),term)

        rv : Z3Term = helper(topterm)
        assert rv is not None, topterm
        # print(f"tern2z3({topterm}) == {rv}. type of input ", type(topterm))
        return rv

    def getActionSeqFromExtra(assignments:TupleLinkedList) -> List[str]:
        seq = dict()
        maxind = 0
        def helper(ll):
            nonlocal maxind
            nonlocal seq
            if ll is None:
                return []
            else:
                x, rest = ll
                if isinstance(x,tuple):
                    key,val = x

                    if cast(str,key).startswith("action"):
                        ind = int(key[7:])
                        seq[ind] = val
                        maxind = max(maxind,ind)
                helper(rest)
        helper(assignments)
        rv = (maxind+1)*['']
        for i in range(0,maxind+1):
            rv[i] = seq[i]
        return rv

    def checkEndOfTraceClaims(core: CoreSymbExecState):
        pathconstr, time_pathconstr, state, t, envvars, extra = core
        actseq = getActionSeqFromExtra(extra.items)
        # print(f"Last event name is {actseq[-1]}")
        # propsubst : Dict[str,bool] = dict()
        # propsubst['last_event_name_is' + actseq[-1]] = True
        # for eventname in prog.actions_by_id:
        #     if eventname == actseq[-1]:
        #         propsubst['last_event_name_is' + eventname] = True
        #     else:
        #         propsubst['last_event_name_is' + eventname] = False
        if len(prog.end_of_trace_claims) > 0:
            claim = term2z3(prog.end_of_trace_claims[0], None, None, core)
            # z3input = Z3.simplify(Z3.And(Z3.Not(claim), core.full_constraint()))
            z3input = Z3.And(Z3.Not(claim), core.full_constraint())
            checkres = check(z3input)
            if str(checkres) != 'unsat':
                print("\n\nstate", str(state))
                print("\n\nz3input", z3input)
                print("\n\nsz3", sz3)
                assert False,  str(checkres) +'\n\n'+ str(z3input) +'\n\n'+ str(actseq)


        # checkres = check(Z3.And(Z3.Not(qp.assertion), qp.core.full_constraint()))

    def checkFinishedTrace(core: CoreSymbExecState):
        pathconstr, time_pathconstr, state, t, envvars, extra = core

        # checkEndOfTraceClaims(core)

        # Check write count bounds
        for v,(low,high) in prog.write_bounds.items():
            cnt = extra.count("write_{v}")
            assert low <= cnt and (high is None or cnt <= high), f"State var {v} write counts not in the range [{low},{high}]"


        # Check that no state variable is changed more than once in any StateTransform section
        written : Set[str] = set()
        if TRACE:
            print("extra...", extra)
        def f(x:Any):
            nonlocal written
            if isinstance(x,str):
                if x.startswith("write_"):
                    varname = x[6:]
                    assert varname not in written, f"Variable {varname} written to more than once in a path through a StateTransform section. This is 'written': {written}. The whole 'extra' dump: {extra}"
                    written.add(varname)
            elif isinstance(x,tuple) and len(x) >= 2 and x[0].startswith("action_"):
                written = set()

        extra.forEach(f)

    def sevalActionAfterStateTransform(a: Action, next_state:Store, core: CoreSymbExecState) -> SEvalRV:
        nonlocal fulfilled_cnt
        pathconstr, time_pathconstr, state, t, envvars, extra = core

        if a.action_id == "IssueSAFEPreferredStock":
            print("\nIssueSAFEPreferredStock.state_transform:\n", a.state_transform)

        if state is not next_state:
            for v in state:
                # print(state)
                if v not in next_state:
                    next_state = next_state.set(v, state[v])

        if a.dest_situation_id == FULFILLED_SITUATION_LABEL:
            fulfilled_cnt += 1
            if TRACE:
                print("Fulfilled")
            checkFinishedTrace(core)
            checkEndOfTraceClaims(core)
            return SEvalRVStopThread("trans to Fulfilled from sevalAction")
        elif a.dest_situation_id.startswith("Breached_"):
            if TRACE:
                print(a.dest_situation_id)
            checkFinishedTrace(core)
            return SEvalRVStopThread("trans to a Breached_* situation from sevalAction")
        else:
            if a.following_anon_situation:
                sit = a.following_anon_situation
            else:
                sit = prog.situation(a.dest_situation_id)

            return sevalSituation(sit,
                                  CoreSymbExecState(pathconstr, time_pathconstr,
                                                    next_state,
                                                    t+1,envvars,extra) )


    def sevalAction(a: Action,
                    actparam_store: Optional[OneUseStore], # if Action has params
                    core: CoreSymbExecState
                   ) -> SEvalRV:
        nonlocal fulfilled_cnt
        pathconstr, time_pathconstr, state, t, envvars, extra = core

        extra = extra.push((f"action_{t}", a.action_id))

        if TRACE:
            # print("Action seq (reversed):", list(reversed(getActionSeq(extra.changes))))
            print("Action seq:", getActionSeqFromExtra(extra.items))

        # SETTING last_event_td, and "deleting" next_event_td
        # envvars = envvars.set("last_event_td", envvars["next_event_td"])
        envvars = envvars.set("last_event_td", name2symbolicvar(f"td_{core.time}", 'TimeDelta', sz3))
        envvars = envvars.set("next_event_td", None)
        envvars = envvars.set("next_event_dt", None)
        envvars = envvars.set("last_event_name", cast(Any,a.action_id))

        assert isinstance(state, LedgerDict), type(state)

        # if a.state_transform:
        res = sevalBlock(a.state_transform.statements if a.state_transform else [], Store({}), a, actparam_store,
                         CoreSymbExecState(pathconstr, time_pathconstr, state, t, envvars, extra))

        # assert isinstance(res, SEvalRVStopThread), res
        # return res
        # assert isinstance(res, SEvalRVChange), res

        if isinstance(res, SEvalRVChange):
            # In this case, we will continue execution via the last return statement of this function
            assert isinstance(res.next_state, LedgerDict), type(res.next_state)
            state = res.next_state
            pathconstr = res.path_constraint
            extra = res.extra
        elif isinstance(res, SEvalRVStopThread):
            # this means all the threads got added to queryPaths, which will happen for example
            # if the last statement in the StateTransform block is an IfElse.
            return res
        else:
            raise NotImplementedError

        return sevalActionAfterStateTransform(a, res.next_state, CoreSymbExecState(pathconstr, time_pathconstr, state, t, envvars, extra))

    def sevalBlock(block: StatementList,
                   next_state: Store,
                   a: Action,
                   actparam_store: Optional[OneUseStore],  # if it's in an Action with params
                   core: CoreSymbExecState
                   ) -> SEvalRV:
        pathconstr, time_pathconstr, state, t, envvars, extra = core
        if TRACE:
            print(f"sevalBlock(...,{state},{next_state}...,{t}")
        assert isinstance(state, LedgerDict), type(state)
        assert isinstance(next_state, LedgerDict), type(next_state)

        if len(block) == 0:
            # we're done evaluating statements. time to copy assignments that didn't change, from state to next_state.
            # minor optimization: if the assignments are identical in memory, then there's certainly nothing to do.
            if state is not next_state:
                for v in state:
                    if v not in next_state:
                        next_state = next_state.set(v, state[v])
            return SEvalRVChange(next_state=next_state,path_constraint=pathconstr,extra=extra)
        else:
            rv1 = sevalStatement(block[0], next_state, a, actparam_store,
                                 CoreSymbExecState(pathconstr, time_pathconstr, state, t, envvars, extra))
            if isinstance(rv1, (SEvalRVInconsistent, SEvalRVStopThread)):
                return rv1
            elif isinstance(rv1, SEvalRVChange):
                # now it is each statement's responsibility to execute the next statement
                return rv1
                # return sevalBlock(block[1:], rv1.next_state,a, actparam_store,
                #                   CoreSymbExecState(rv1.path_constraint, time_pathconstr, state, t, envvars, rv1.extra ))
            else:
                raise Exception(rv1)

    def sevalStatement(stmt: Statement,
                       next_state:Store,
                       a:Action,
                       actparam_store: Optional[OneUseStore], # if it's in an Action with params
                       core: CoreSymbExecState
                      ) -> SEvalRV:
        pathconstr, time_pathconstr, state, t, envvars, extra = core
        if TRACE:
            print(f"sevalStatement(\n{stmt.toStr(1)},{state},{next_state},...,{t})")
        if isinstance(stmt, StateVarAssign):
            z3term = term2z3(stmt.value_expr, next_state, actparam_store, core)
            next_state = next_state.set(stmt.varname, z3term)
            extra = extra.push(f"write_{stmt.varname}")

            # thread continues immediately
            # return SEvalRVChange(next_state, pathconstr, extra)
            todo_once("suspiciously duplicated logic")
            next = stmt.next_statement()
            # if stmt.varname == 'conversion_price':
            #     print("(conversion_price assign Statement)", stmt)
            #     print("(conversion_price assign Statement).next_statement():", next)
            #     print("(conversion_price assign Statement).parent_block:", stmt.parent_block)
            #     print("(conversion_price assign Statement).parent_block.index(stmt):", stmt.parent_block.index(stmt))
            # if stmt.varname == "conversion_price":
            #     print("write statement", stmt)
            #     print("next is", next)

            if next:
                return sevalStatement(next, next_state, a, actparam_store,
                                      CoreSymbExecState(core.path_constraint, core.time_path_constraint, core.state, core.time, core.env_vars,
                                                        extra))
            else:
                # when `stmt` is a terminal statement in the StateTransform section
                return sevalBlock([], next_state, a, actparam_store,
                                   CoreSymbExecState(core.path_constraint,
                                                     core.time_path_constraint,
                                                     state,
                                                     core.time,
                                                     core.env_vars,
                                                     extra))
                # return sevalAction(a, actparam_store, True,
                #                    CoreSymbExecState(core.path_constraint,
                #                                      core.time_path_constraint,
                #                                      next_state,
                #                                      core.time,
                #                                      core.env_vars,
                #                                      extra))

            # return SEvalRVChange(next_state, pathconstr, extra)

        elif isinstance(stmt, IfElse):
            test = term2z3(stmt.test, next_state, actparam_store, core)

            addQueryPath(
                GuardedBlockPath(stmt.true_branch, next_state, stmt.next_statement(), a, actparam_store,
                                 CoreSymbExecState(conj(test, pathconstr), time_pathconstr, state, t, envvars, extra)) )

            # if stmt.false_branch:
            #     print("have false branch")
            addQueryPath(
                GuardedBlockPath(stmt.false_branch or [], next_state, stmt.next_statement(), a, actparam_store,
                                 CoreSymbExecState(conj(neg(test), pathconstr), time_pathconstr, state, t, envvars, extra)) )

            # threads are continued via the GuardedBlockPath tasks
            return SEvalRVStopThread("from sevalStatement")

        elif isinstance(stmt, FVRequirement):
            # raise NotImplementedError
            assertion = term2z3(stmt.value_expr, next_state, actparam_store, core)
            addQueryPath(
                AssertionPath(assertion, stmt.next_statement(), next_state, a, actparam_store,
                              CoreSymbExecState(pathconstr, time_pathconstr, state, t, envvars, extra)))

            # thread is continued via the AssertionPath task
            return SEvalRVStopThread("from sevalStatement")

        elif isinstance(stmt, LocalVarDec):
            raise Exception(f"You need to use eliminate_local_vars before doing symbolic execution. Problem statement is:\n{stmt}.")

        else:
            raise MyTypeError(Statement, stmt)

    def sevalSituation(s: Situation,
                       core: CoreSymbExecState
                      ) -> SEvalRV:

        pathconstr, time_pathconstr, state, t, envvars, extra = core

        # SETTING last_situation_td
        # envvars = envvars.set("last_situation_td", envvars["last_event_td"])
        envvars = envvars.set("last_situation_td", name2symbolicvar(f"td_{core.time}", 'TimeDelta', sz3))

        if TRACE:
            print(f"sevalSituation({s.situation_id},{state},...,{t})")
        assert isinstance(state, LedgerDict), type(state)
        todo_once("QueryPath for each precondition, or their conjunction")

        for rule in s.action_rules():
            assert isinstance(rule,EventRule)
            if rule.entrance_enabled_guard:
                enabled_guard_z3 = term2z3(rule.entrance_enabled_guard, None, None,
                                           CoreSymbExecState(pathconstr, time_pathconstr, state, t, envvars, extra))
                addQueryPath(EventRuleEnabledPath( rule,
                                                    CoreSymbExecState(conj(enabled_guard_z3, pathconstr),
                                                                      time_pathconstr, state, t, envvars, extra)
                ))
            else:
                res = sevalRuleIgnoreGuard(rule, CoreSymbExecState(pathconstr, time_pathconstr, state, t, envvars, extra))
                if isinstance(res, (SEvalRVStopThread, SEvalRVInconsistent)):
                    continue
                elif isinstance(res, SEvalRVChange):
                    raise NotImplementedError
                else:
                    raise NotImplementedError

        return SEvalRVStopThread("from sevalSituation")

    def sevalRuleIgnoreGuard(rule:EventRule, core:CoreSymbExecState) -> SEvalRV:
        pathconstr, time_pathconstr, state, t, envvars, extra = core
        assert isinstance(state, LedgerDict)
        action = prog.action(rule.action_id)
        # first need to handle the new action params
        actparam_store: OneUseStore
        if rule.fixed_args:
            assert not rule.where_clause
            d = {actparam_name: rule.fixed_args[
                action.param_name_to_ind[actparam_name]] for actparam_name in action.param_names}
            actparam_store = OneUseStore({actparam_name: term2z3(d[actparam_name], state, None, core) for actparam_name in d})
        else:
            actparam_store = OneUseStore({actparam_name:
                name2actparam_symbolic_var(actparam_name,t,action.param_sorts_by_name[actparam_name], sz3)\
                                          for actparam_name in action.param_names})

        # SETTING next_event_td
        envvars = envvars.set("next_event_td", name2symbolicvar(f"td_{t+1}",'TimeDelta',sz3))
        envvars = envvars.set("next_event_dt", name2symbolicvar(f"td_{t+1}", 'TimeDelta', sz3) - name2symbolicvar("td_0", 'TimeDelta', sz3)) # type:ignore
        if t > 0:
            time_pathconstr = conj(
                name2symbolicvar(f"td_{t-1}", 'TimeDelta', sz3) <= name2symbolicvar(f"td_{t}", 'TimeDelta', sz3), # type:ignore
                time_pathconstr)

        # raise Exception("Need to include type assumptions. should I keep them separate from pathconstr? then they won't clutter the middle of the z3 input formula.")

        if rule.time_constraint is None and rule.where_clause is None:
            return sevalRuleIgnoreGuardAndParamConstraints(rule, actparam_store, CoreSymbExecState(pathconstr, time_pathconstr, state, t, envvars, extra))
        else:
            newcore = CoreSymbExecState(pathconstr, time_pathconstr, state, t, envvars, extra)
            wc : Z3Term = Z3TRUE
            ruleparam_store : Optional[OneUseFrozenDict] = None
            if rule.where_clause and rule.ruleparam_to_ind:
                ruleparam_store = OneUseStore({ruleparam_name:
                                               name2actparam_symbolic_var(ruleparam_name, t,
                                                                          action.param_sort(ruleparam_ind), sz3) \
                                               for (ruleparam_name, ruleparam_ind) in rule.ruleparam_to_ind.items()})
                wc = term2z3(rule.where_clause, None, ruleparam_store, newcore)
            tc : Z3Term = Z3TRUE
            if rule.time_constraint and rule.time_constraint != "no_time_constraint":
                tc = term2z3(rule.time_constraint, None, ruleparam_store, newcore)

            addQueryPath( EventRuleParamsConstraintPath(
                rule, actparam_store, CoreSymbExecState(conj(wc, pathconstr), conj(tc, time_pathconstr), state, t, envvars, extra)
            ))
            return SEvalRVStopThread("from sevalRuleIgnoreGuard")


    def sevalRuleIgnoreGuardAndParamConstraints(
                             rule:EventRule,
                             actparam_store: OneUseStore,
                             core: CoreSymbExecState) -> SEvalRV:
        if TRACE:
            print(f"sevalRuleIgnoreGuardAndParamConstraints({rule} ; {core.state})")
        assert isinstance(core.state, LedgerDict)
        action = prog.action(rule.action_id)
        return sevalAction(action, actparam_store, core)

    def check(formula:Z3Term) -> Z3.CheckSatResult:
        sz3.push()
        sz3.add(chcast(Z3.ExprRef,formula))
        rv = sz3.check()
        if rv == Z3.unknown:
            print("Reason unknown:", sz3.reason_unknown())
            print("Z3 gave up on this query:\n\n", sz3.to_smt2())
            # sz3.pop()
            # sz3.push()
            # simplified = Z3.simplify(chcast(Z3.ExprRef,formula))
            # sz3.add(simplified)
            # rv2 = sz3.check()
            #
            # if rv2 == Z3.unknown:
            #     # print("Z3 gave up (twice, once post-simplify) on this query:\n\n", sz3.to_smt2())
            #     print("Z3 gave up (twice, once post-simplify) on this query:\n\n", formula)
            # sz3.pop()
            # return rv2
        sz3.pop()
        return rv

    def query(qp:QueryPath) -> SEvalRV:

        if isinstance(qp, GuardedBlockPath):
            checkres = check(qp.core.full_constraint())

            if checkres == Z3.sat:# or checkres == Z3.unknown:
                res = sevalBlock(qp.block, qp.next_state, qp.action, qp.action_params, qp.core)
                if isinstance(res, SEvalRVChange):
                    if qp.next_statement:
                        return sevalStatement(qp.next_statement, res.next_state, qp.action, qp.action_params,
                                           CoreSymbExecState(res.path_constraint,
                                                             qp.core.time_path_constraint,
                                                             res.next_state,
                                                             qp.core.time,
                                                             qp.core.env_vars,
                                                             res.extra))
                    else:
                        # case when the ifelse is the last statement in its block
                        return sevalActionAfterStateTransform(qp.action, res.next_state,
                                CoreSymbExecState(res.path_constraint,
                                                  qp.core.time_path_constraint,
                                                  # qp.core.state, ??
                                                  res.next_state,
                                                  qp.core.time,
                                                  qp.core.env_vars,
                                                  res.extra))

                # elif isinstance(res, (SEvalRVStopThread, SEvalRVInconsistent)):
                #     return res

                elif isinstance(res, SEvalRVStopThread):
                    return res
                else:
                    assert False, res
                # # making this case explicit even though doesn't need to be cuz doesn't hurt and makes it clearer
                # elif isinstance(res, SEvalRVInconsistent):
                #     raise res
                # else:
                #     raise NotImplementedError(str(res))


            elif checkres == Z3.unsat:
                if qp.next_statement:
                    return sevalStatement(chcast(Statement, qp.next_statement), chcast(LedgerDict, qp.next_state),
                                          qp.action, qp.action_params, qp.core)
                else:
                    return sevalActionAfterStateTransform(qp.action, qp.next_state, qp.core)
                # return SEvalRVInconsistent("GuardedBlockPath query unsat")

        elif isinstance(qp, EventRuleEnabledPath):
            checkres = check(qp.core.full_constraint())
            if checkres == Z3.sat:# or checkres == Z3.unknown:
                return sevalRuleIgnoreGuard(qp.rule, qp.core)
            elif checkres == Z3.unsat:
                return SEvalRVInconsistent("EventRuleEnabledPath query unsat")

        elif isinstance(qp, EventRuleParamsConstraintPath):
            assert isinstance(qp.core.state, LedgerDict)
            # print("z3 query", qp.core.path_constraint)
            checkres = check(qp.core.full_constraint())
            # invalid
            # syntax( < string >, line
            # 1)
            if checkres == Z3.sat:# or checkres == Z3.unknown:
                if TRACE:
                    print("path constraint sat:", z3termPrettyPrint(qp.core.full_constraint()))
                return sevalRuleIgnoreGuardAndParamConstraints(qp.rule, qp.action_params, qp.core)
            elif checkres == Z3.unsat:
                if TRACE:
                    print("path constraint unsat:", z3termPrettyPrint(qp.core.full_constraint()))
                return SEvalRVInconsistent("EventRuleParamsConstraintPath query unsat")

        elif isinstance(qp, AssertionPath):
            checkres = check(Z3.And(Z3.Not(qp.assertion), qp.core.full_constraint()))
            if checkres == Z3.sat:
                raise Exception(f"Negation of assertion is satisfiable!:\n{qp}")
            if checkres == Z3.unknown:
                print(f"Failed to prove assertion:\n{qp}\nReason unknown:{sz3.reason_unknown()}")
                addUnprovedAssertion(qp)
                # return SEvalRVStopThread('')
            if qp.next_statement:
                return sevalStatement(chcast(Statement,qp.next_statement),chcast(LedgerDict,qp.next_state),qp.action,qp.action_params,qp.core)
            else:
                return sevalActionAfterStateTransform(qp.action, qp.next_state, qp.core)

            # raise NotImplementedError("no assertions till working without them")

        else:
            raise NotImplementedError(qp)

        assert checkres == Z3.unknown, checkres
        if TRACE:
            if checkres == Z3.unknown:
                print(f"UNKNOWN: {qp.core.full_constraint()}")
            # if checkres_time == Z3.unknown:
            #     print(f"UNKNOWN (time): {qp.core.full_constraint()}")
        # addTimeoutQueryPath(qp) # done in query()
        # sz3.push()
        # sz3.add(simplified)
        # print("Try again:",sz3.check(simplified), sz3.reason_unknown())
        # sz3.pop()

        addTimeoutQueryPath(qp)
        return SEvalRVStopThread("timeout on " + str(qp))
        # return SEvalRVTimeout(str(qp.core.path_constraint))
        # raise NotImplementedError("don't want to be dealing with timeouts till working with easy examples")

        # raise Exception("can't get here?", qp)

    def pathChooser():

        while len(queryPaths) > 0:

            # print("#paths: ", len(queryPaths), len(timedoutQueryPaths), len(unprovedAssertions))
            qpath = takeQueryPath()
            if TRACE:
                print("popping query path ", str(qpath))
            # print("paths: ", len(queryPaths), qpath)

            # this call will add paths to queryPaths:
            res = query(qpath)
            if TRACE:
                print("query result:", res)
            # we don't need to do anything in the other two SEvalRV cases?

    # raise Exception("Before I do anything else, assert the types of all the contract params")

    for_contractParams : Dict[str,Z3Term] = dict()
    cdec : ContractParamDec
    for paramname,cdec in prog.contract_params.items():
        if cdec.value_expr is None or not USE_CONTRACT_PARAM_VALS:
            for_contractParams[paramname] = name2symbolicvar(paramname, cdec.sort, sz3)
        else:
            assert isinstance(cdec.value_expr, Literal), "This shouldn't be a constraint. Ask Dustin to fix it. Sorry."
            # RE prev line... why didn't I (or don't I) just use term2z3?
            for_contractParams[paramname] = somewhatPrimValToZ3(cdec.value_expr.lit, prog.timeunit)

    contractParams = OneUseFrozenDict[str,Z3Term](for_contractParams)
    # print("for_contractParams:", for_contractParams)
    envvars: Store = Store({
        "last_event_td": name2symbolicvar('td_0',"TimeDelta", sz3),
        "last_situation_td": name2symbolicvar('td_0',"TimeDelta", sz3)
    })
    time_pathconstr = cast(Z3Term,
                           name2symbolicvar('td_0','TimeDelta', sz3) == Z3.RealVal(0))
    pathconstr = Z3.BoolVal(True)
    # pathconstr = conj(*[equals(name, term) for name,term in for_contractParams.items()])
    extra = PushOnlyStack[Any]()
    startstate : Store = Store({})
    dec : StateVarDec
    for dec in prog.state_var_decs.values():
        if dec.initval:
            startstate = startstate.set(dec.name,
                                        term2z3(dec.initval, None, None,
                                                CoreSymbExecState(pathconstr, time_pathconstr, startstate, 0, envvars, extra)))
    # print("\n\nstartstate",startstate)
    startcore = CoreSymbExecState(pathconstr, time_pathconstr, startstate, 0, envvars, extra)

    sevalSituation(prog.situation(prog.start_situation_id), startcore)
    pathChooser()

    print(f"Paths ending in Fulfilled: {fulfilled_cnt}")
