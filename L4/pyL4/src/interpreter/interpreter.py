from datetime import datetime
import logging

from src.parse_to_model.sexpr_to_L4Contract import primed, isprimed, unprimed
from src.independent.util import todo_once, chcast, contract_bug, castid
from src.independent.util_for_dicts import hasNotNone
from src.independent.util_for_dicts import dictInc
from src.constants_and_defined_types import LOOP_KEYWORD, LocalVarSubst, ENV_ROLE
from src.constants_and_defined_types import TIME_CONSTRAINT_OPERATORS, TIME_CONSTRAINT_PREDICATES, \
    ContractParamId, SituationId, ActionParamSubstList, \
    Data, GVarSubst, ContractParamSubst
from src.interpreter.interpreter_support import *
from src.model.Action import Action
from src.model.EventRule import ActorEventRule, DeadlineEventRule, EventRule
from src.model.BoundVar import StateVar, ContractParam, ActionBoundActionParam, \
    RuleBoundActionParam, LocalVar, PrimedStateVar
from src.model.ContractParamDec import ContractParamDec
from src.model.EventsAndTraces import Event, Trace, breachSituationId
from src.model.StateTransform import StateTransform
from src.model.Statement import IfElse, StateVarAssign, LocalVarDec
from src.model.StateVarDec import StateVarDec
from src.model.L4Contract import L4Contract
from src.model.Literal import Literal, SimpleTimeDeltaLit, RoleIdLit
from src.model.Situation import Situation
from src.model.Term import FnApp

logging.basicConfig(
    format="[%(levelname)s] %(funcName)s: %(message)s",
    level=logging.INFO )


class ExecEnv:
    # All the instance variables of ExecEnv should probably be prefixed with _ to indicate not to use access them
    # outside of the ExecEnv class.
    # But I don't actually care if you access them outside the ExecEnv class. It's not that kind of class.
    # So, I'm prefixing none of them.

    def __init__(self, prog:L4Contract) -> None:
        self.top : L4Contract = prog
        self.contract_param_vals: ContractParamSubst = dict()

        self.localvar_vals: LocalVarSubst = dict()
        self.gvarvals: GVarSubst = dict()
        self.gvar_write_cnt : Dict[StateVarId, int] = dict()

        # following 2 change only in apply_action:
        self.last_or_current_situation_id: SituationId = prog.start_situation_id
        self.last_appliedaction_params : Optional[ActionParamSubstList] = None
        # following changes only in evalTrace
        self.cur_event : Optional[Event] = None

        self.evaluation_is_in_action = False
        self.evaluation_is_in_situation = False
        self.evaluation_is_in_next_action_rule = False

        self.writeouts : Dict[str,Data] = dict()

        # self.start_datetime = datetime.now(timezone.utc)
        # self.start_datetime : datetime = datetime(2000,1,1,0,0,0,0,timezone.utc)
        self.start_datetime: datetime = prog.start_datetime or datetime(2000, 1, 1, 0, 0, 0, 0)
        self.absolute_timeint2timedelta_converter = self.getIntToDeltaConverter()
        self.last_situation_entrance_delta : timedelta = timedelta(0) # = self.timeint2delta(0)




    def writeout(self, varname:str, data:Data):
        assert varname not in self.writeouts
        self.writeouts[varname] = data

    def getIntToDeltaConverter(self) -> Callable[[int], timedelta]:
        if self.top.timeunit == 'd':
            def f(x:int) -> timedelta:
                return timedelta(days=x)
            return f
        elif self.top.timeunit == 'h':
            def f(x:int) -> timedelta:
                return timedelta(hours=x)
            return f
        elif self.top.timeunit == 'w':
            def f(x:int) -> timedelta:
                return timedelta(weeks=x)
            return f
        elif self.top.timeunit == 'm':
            def f(x:int) -> timedelta:
                return timedelta(minutes=x)
            return f
        elif self.top.timeunit == 'ms':
            def f(x:int) -> timedelta:
                return timedelta(milliseconds=x)
            return f
        else:
            assert self.top.timeunit == 's'
            def f(x:int) -> timedelta:
                return timedelta(seconds=x)
            return f

    def delta2datetime(self, td:timedelta) -> datetime:
        return self.start_datetime + td

    def datetime2delta(self, dt:datetime) -> timedelta:
        return dt - self.start_datetime

    def cur_event_datetime(self) -> datetime:
        assert self.cur_event is not None
        return self.delta2datetime(self.cur_event_delta())

    def cur_event_delta(self) -> timedelta:
        assert self.cur_event is not None
        return self.absolute_timeint2timedelta_converter(self.cur_event.timestamp)

    @property
    def last_or_current_situation(self) -> Situation:
        return self.top.situation(self.last_or_current_situation_id)

    def evalTrace(self, trace:Trace, finalSituationId:Optional[SituationId] = None, final_var_vals: Optional[GVarSubst] = None, verbose=False, debug=False):
        prog = self.top
        self.evalContractParamDecs(prog.contract_params)
        self.evalStateVarDecs(prog.state_var_decs)
        for i in range(len(trace)):
            # print("Environ", self.environ_tostr())
            eventi = trace[i]
            self.cur_event = eventi
            cur_action_id = self.cur_event.action_id
            cur_event_delta = self.cur_event_delta()
            cur_event_datetime = self.delta2datetime(cur_event_delta)
            cur_action = self.top.action(cur_action_id)

            for gvarid in self.gvarvals:
                if isprimed(gvarid):
                    self.gvarvals[unprimed(gvarid)] = self.gvarvals[gvarid]
            self.gvarvals = {id:self.gvarvals[id] for id in self.gvarvals if not isprimed(id)}
            # print("last_situation_entrance_delta", self.last_situation_entrance_delta)

            if self.last_situation_entrance_delta is not None and self.last_situation_entrance_delta > cur_event_delta:
                self.evalError(f"Event timestamps must be non-decreasing, but last event timestamp "
                               f"{self.last_situation_entrance_delta} is greater than current event timestamp {cur_event_delta}")

            if not cur_action: self.evalError(f"Don't recongize action id {cur_action_id}")

            if not self.top.situation_mentions_action_in_nextaction_rule(self.last_or_current_situation_id, cur_action_id):
                self.evalError(  f"Action type indicated using a next-rule, but current situation {self.last_or_current_situation_id} "
                                 f"has no such rule for action {cur_action_id}." )
            self.evaluation_is_in_situation = True
            nextrule_assessment = self.assess_event_legal_wrt_nextrules(eventi, verbose=verbose)
            self.evaluation_is_in_situation = False

            actionstr = event_to_action_str(eventi)
            srcid = self.last_or_current_situation_id
            if isinstance(nextrule_assessment, EventOk):
                self.evaluation_is_in_action = True
                self.apply_action(cur_action,verbose)
                self.evaluation_is_in_action = False
                if verbose:
                    print(f"[{cur_event_datetime}] {srcid} --{actionstr}--> {self.last_or_current_situation_id}\n")
            elif isinstance(nextrule_assessment, BreachResult):
                if verbose:
                    print("Breach result:", nextrule_assessment)
                breach_situation_id = breachSituationId(*nextrule_assessment.role_ids)
                self.apply_action(cur_action, verbose, to_breach_situation_id = breach_situation_id)

                if verbose:
                    print(f"[{cur_event_datetime}] {srcid} --{actionstr}--> {breach_situation_id}")

                # if i != len(trace) - 1:
                #     print("Trace prefix results in a breach, but there are more events after.")
                assert i == len(trace) - 1, "Trace prefix results in a breach, but there are more events after."
                break
            else:
                assert False, "Can't get here?"
            if debug:
                # code, IPython, pdb are other options
                import ipdb   # type: ignore
                ipdb.set_trace()  # type: ignore
                # other things tried:
                # code.InteractiveConsole(locals=locals()).interact()
                # IPython.embed()

        for gvarid in self.gvarvals:
            if isprimed(gvarid):
                self.gvarvals[unprimed(gvarid)] = self.gvarvals[gvarid]
        self.gvarvals = {id: self.gvarvals[id] for id in self.gvarvals if not isprimed(id)}

        if final_var_vals:
            for varid,expected_val in final_var_vals.items():
                if varid in self.gvarvals:
                    actual_val = self.gvarvals[castid(StateVarId, varid)]
                elif varid in self.writeouts:
                    actual_val = self.writeouts[varid]
                else:
                    self.evalError(f"Var {varid} in Trace object is neither a state variable nor a writeout local variable")
                assert actual_val == expected_val, f"Expected state variable {gvarid} to have value {expected_val} at end of trace, but had value {actual_val}."

        if finalSituationId:
            assert self.last_or_current_situation_id == finalSituationId, f"Trace expected to end in situation {finalSituationId} but ended in situation {self.last_or_current_situation_id}"


    def assess_event_legal_wrt_nextrules(self, event:Event, verbose=True) -> EventLegalityAssessment:

        enabled_strong_obligs : List[ActorEventRule] = list()

        enabled_permissions : List[ActorEventRule] = list()
        enabled_env_action_rules : List[DeadlineEventRule] = list()

        enabled_weak_obligs_by_role : Dict[RoleId, List[ActorEventRule]] = dict()
        enabled_weak_obligs = list()

        nar: EventRule

        for nar in self.last_or_current_situation.action_rules():
            entrance_enabled = not nar.entrance_enabled_guard or chcast(bool, self.evalTerm(nar.entrance_enabled_guard))

            if entrance_enabled:
                if isinstance(nar, DeadlineEventRule):
                    enabled_env_action_rules.append(nar)
                else:
                    assert isinstance(nar, ActorEventRule)
                    if nar.deontic_keyword == 'may' or nar.deontic_keyword == 'should':
                        enabled_permissions.append(nar)
                    elif nar.deontic_keyword == 'must':
                        enabled_strong_obligs.append(nar)
                    elif nar.deontic_keyword == 'quasi-responsibility':
                        enabled_weak_obligs.append(nar)
                        for role_id in nar.role_ids:
                            if role_id not in enabled_weak_obligs_by_role:
                                enabled_weak_obligs_by_role[role_id] = [nar]
                            else:
                                enabled_weak_obligs_by_role[role_id].append(nar)
                    else:
                        assert False

        # assert len(enabled_weak_obligs_by_role) == 0, enabled_weak_obligs_by_role

        # =====================================================
        # CASE 1: 1 or more entrance-enabled strong obligations
        # =====================================================

        if len(enabled_strong_obligs) > 1:
            contract_bug("There are multiple enabled strong obligations. This is a contract bug that eventually will be ruled out statically.")
            # return ContractFlawedError()
        if len(enabled_strong_obligs) == 1:
            if len(enabled_weak_obligs) > 0 or len(enabled_permissions) > 0 or len(enabled_env_action_rules) > 0:
                contract_bug("There is exactly one enabled strong obligation, but there is also at least one "
                             "enabled permission, environment-action, or weak obligation."
                             "This is a contract bug that eventually will be ruled out statically.")
                # return ContractFlawedError()
            else:
                assert isinstance(enabled_strong_obligs[0],ActorEventRule)
                if self.current_event_compatible_with_enabled_EventRule(enabled_strong_obligs[0]):
                    return EventOk()
                else:
                    return BreachResult(enabled_strong_obligs[0].role_ids,
                                 f"The unique enabled strong obligation (of {enabled_strong_obligs[0].role_ids}) is incompatible "
                                 "with the current event (expired or not yet active, wrong action, wrong role, etc):"
                                 f"\t{event}\n"
                                 f"See rule {enabled_strong_obligs[0]}\n with time constraint {enabled_strong_obligs[0].time_constraint}."
                                 f"Environment looks like:\n" +
                                 self.environ_tostr())

        # ===============================================================
        # CASE 2: 0 entrance-enabled strong obligations (SO action-rules)
        # ===============================================================

        # --------------------------------------------------------------------------------
        # CASE 2a: the current event is compatible with some permission or env action rule
        # --------------------------------------------------------------------------------
        todo_once("Verbose print option here for showing compatible enabled env actions and permissions")
        # We ignore permissions for roles that are not the current role id, for actions that aren't the current
        # action id, whose time constraint is false, or whose where clause is false
        compat_enabled_permissions = list(filter(self.current_event_compatible_with_enabled_EventRule, enabled_permissions))
        if len(compat_enabled_permissions) > 0:
            return EventOk()
        # Similarly for Env actions:
        compat_enabled_env_action_rules = list(filter(self.current_event_compatible_with_enabled_EventRule, enabled_env_action_rules))
        if len(compat_enabled_env_action_rules) > 0:
            return EventOk()

        # -----------------------------------------------------------------------------------
        # CASE 2b: the current event is NOT compatible with any permission or env action rule
        # -----------------------------------------------------------------------------------

        # Case 2b1: there are actually no enabled weak obligations. Thus the current event is not allowed,
        # so we blame the subject of the current event.
        if len(enabled_weak_obligs) == 0:
            return BreachResult([event.role_id],
                                f"Role {event.role_id} attempted an unpermitted action {event.action_id}({event.actionparam_subst_list})")
            # contract_bug(f"No rules apply to the current event\n{event}.\n"
            #              "This is a contract bug that eventually will be ruled out statically.")


        # Case 2b2: weak obligations are relevant even if they are not compatible with `event`,
        # since in that case they can result in a breach. Let's see who has weak obligations that matchTerm
        # the current event, by "filtering out" those that don't.
        # if there are any left, for and role, then all is well.
        for roleid_with_wo in enabled_weak_obligs_by_role:
            for rule in enabled_weak_obligs_by_role[roleid_with_wo]:
                # print(f"An enabled weak oblig rule for {roleid_with_wo}: " + str(rule))
                if self.current_event_compatible_with_enabled_EventRule(rule):
                    if verbose:
                        print("weak oblig rule checks out: ", rule)
                    return EventOk()
            # enabled_weak_obligs_by_role[roleid_with_wo] = list(filter( compat_checker, enabled_weak_obligs_by_role[roleid_with_wo] ))
            # if len(enabled_weak_obligs_by_role[roleid_with_wo]) > 0:
            #     return EventOk()

        # Case 2b3: all the roles (and there's at least one) who had an enabled weak oblig are jointly responsible for the breach
        breach_roles = list(enabled_weak_obligs_by_role.keys())
        return BreachResult(list(breach_roles),
                            f"Role(s) {list(breach_roles)} had weak obligations that went unfulfilled:\n" + str(enabled_weak_obligs_by_role))


    # Only if the current situation is an anonymous situation (i.e. given by a FollowingSituation declaration) is it possible
    # for the where_clause of action_rule to contain action-bound action parameters.
    def current_event_compatible_with_enabled_EventRule(self, action_rule:EventRule)  -> bool:
        assert self.cur_event is not None
        rv : bool
        self.evaluation_is_in_next_action_rule = True
        if isinstance(action_rule,DeadlineEventRule):
            rv = self.cur_event.role_id == ENV_ROLE
        else:
            role_action_match = self.cur_event.role_id in action_rule.role_ids and self.cur_event.action_id == action_rule.action_id
            if not role_action_match:
                rv = False
            elif not self.evalTimeConstraint(action_rule.time_constraint):
                rv = False
            elif action_rule.where_clause:
                rv = chcast(bool,self.evalTerm(action_rule.where_clause))
            elif action_rule.param_setter:
                if self.cur_event.actionparam_subst_list:
                    argvals = [self.evalTerm(arg) for arg in action_rule.param_setter]
                    rv = all(argvals[i] == self.cur_event.actionparam_subst_list[i] for i in range(len(self.cur_event.actionparam_subst_list)))
                else:
                    rv = len(action_rule.param_setter) == 0
            else:
                rv = True
        self.evaluation_is_in_next_action_rule = False
        return rv

    def environ_tostr(self) -> str:
        return f"{str(self.gvarvals)}\n{str(self.last_appliedaction_params)}\n{str(self.contract_param_vals)}"

    def apply_action(self, action:Action, verbose=True, to_breach_situation_id:Optional[SituationId] = None):
        assert self.cur_event is not None

        if to_breach_situation_id is not None:
            self.last_or_current_situation_id = to_breach_situation_id
            self.last_situation_entrance_delta = self.cur_event_delta()
            return
        self.last_appliedaction_params = self.cur_event.actionparam_subst_list

        if action.state_transform:
            self.evalCodeBlock(action.state_transform,verbose)
        if action.dest_situation_id != LOOP_KEYWORD:
            self.last_or_current_situation_id = action.dest_situation_id
        self.last_situation_entrance_delta = self.cur_event_delta()
        return


    def evalStateVarDecs(self, decs : Dict[StateVarId, StateVarDec]):
        for (var,dec) in decs.items():
            # print("dec: ", dec, dec.initval)
            if dec.initval is None:
                self.gvarvals[var] = None
                # print(f"Global var {var} has no initial value.")
                dictInc(self.gvar_write_cnt, var, init=0)
            else:
                self.gvarvals[var] = self.evalTerm(dec.initval)
                # print(f"Global var {var} has initial value {self.gvarvals[var]}.")
                dictInc(self.gvar_write_cnt, var, init=1)
            # print('evalStateVarDecs: ', var, dec, self.gvar_write_cnt[var])

    def evalContractParamDecs(self, decs : Dict[ContractParamId, ContractParamDec]):
        for (name,dec) in decs.items():
            # print("contract param dec", dec, type(dec.value_expr))
            if not(dec.value_expr is None):
                self.contract_param_vals[name] = self.evalTerm(dec.value_expr)
            else:
                self.contract_param_vals[name] = None
            # print(name, type(self.contract_param_vals[name]))


    def evalCodeBlock(self, transform:StateTransform, verbose:bool):
        for statement in transform.statements:
            self.evalStatement(statement,verbose)

    def evalStatement(self, stmt:Statement, verbose:bool):
        # An Action's StateTransform block is *always* evaluated in the most recent state variable and
        # action parameter substitution context that's visible to it, as given by self.gvarvals and self.cur_event,
        # even when applying an action from a PartlyInstantiatedPartyFutureEventRule.
        # Therefore, this function does not take an EvalContext argument.

        if isinstance(stmt, StateVarAssign):
            gvardec = stmt.vardec
            assert gvardec is not None, f"Global variable declaration for {stmt.varname} not found."

            todo_once("kill writeOnceMore")
            if gvardec.isWriteOnceMore() and self.gvar_write_cnt[stmt.varname] >= 2:
                raise Exception(f"Attempt to write twice more (after init) to writeOnceMore variable `{stmt.varname}`")
            dictInc(self.gvar_write_cnt, stmt.varname, init=1)
            todo_once("use vardec for runtime type checking")
            rhs_value = self.evalTerm(stmt.value_expr)

            if stmt.varop == ":=":
                self.gvarvals[primed(stmt.varname)] = rhs_value
            else:
                current_var_val: Data = self.gvarvals[stmt.varname]
                if stmt.varop == "+=":
                    self.gvarvals[stmt.varname] = current_var_val + rhs_value
                elif stmt.varop == "-=":
                    self.gvarvals[stmt.varname] = current_var_val - rhs_value
                elif stmt.varop == "*=":
                    self.gvarvals[stmt.varname] = current_var_val * rhs_value
                else:
                    raise Exception('fixme')
            if verbose:
                print(f"\t{stmt.varname} ← {self.gvarvals[stmt.varname]}")

        elif isinstance(stmt, FVRequirement):
            # print("conjecture " + str(stmt))
            assert self.evalTerm(stmt.value_expr), f"""Conjecture {stmt.value_expr} is false! Variable values and action params:
            {str(self.gvarvals)}
            {str(self.last_appliedaction_params)}
            """

        elif isinstance(stmt, IfElse):
            test_result = chcast(bool, self.evalTerm(stmt.test))
            if test_result:
                self.evalCodeBlock(StateTransform(stmt.true_branch), verbose)
            elif stmt.false_branch:
                self.evalCodeBlock(StateTransform(stmt.false_branch), verbose)

        elif isinstance(stmt, LocalVarDec):
            rhs_value = self.evalTerm(stmt.value_expr)
            self.localvar_vals[stmt.varname] = rhs_value
            if stmt.is_writeout:
                todo_once("Writeouts don't work currently because of local var elim.")
                self.writeout(stmt.varname, rhs_value)

        else:
            raise NotImplementedError("Unhandled Statement: " + str(stmt))


    def evalTerm(self,
                 term:Term) -> Any:
        assert term is not None
        # assert self.cur_event is not None  # nope! it can be None when evaluating terms in contract param dec and state var decs

        rv : Any = None

        # next_event_timestamp will be None when evaluating an entrance-guard
        try:
            if isinstance(term, FnApp):
                rv = self.evalFnApp(term)

            elif isinstance(term, Literal):
                rv = self.evalLit(term)

            elif isinstance(term, StateVar):
                assert term.name in self.gvarvals and self.gvarvals[term.name] is not None, f"We don't know the current-var-value {term.name}."
                rv = self.gvarvals[term.name]

            elif isinstance(term, PrimedStateVar):
                assert term.name in self.gvarvals and self.gvarvals[term.name] is not None, f"We don't know the next-var-value {term.name}."
                rv = self.gvarvals[term.name]

            elif isinstance(term, ContractParam):
                assert hasNotNone(self.contract_param_vals, term.name), term.name
                rv = self.contract_param_vals[term.name]

            elif isinstance(term, ActionBoundActionParam):
                assert (self.last_appliedaction_params is not None) and term.ind < len(self.last_appliedaction_params), \
                    f"Action-bound action parameter {term} (index {term.ind}) occurrence but only have " \
                    f"{len(self.last_appliedaction_params) if self.last_appliedaction_params else 0} values supplied."
                rv = self.last_appliedaction_params[term.ind]

            elif isinstance(term, RuleBoundActionParam):
                assert self.cur_event and self.cur_event.actionparam_subst_list is not None, f"Expected current event {self.cur_event} to have an action parameter named {term}"
                rv = self.cur_event.actionparam_subst_list[term.ind]

            elif isinstance(term, SimpleTimeDeltaLit):
                rv = term.lit

            elif isinstance(term, LocalVar):
                rv = self.localvar_vals[term.name]

            else:
                contract_bug(f"evalTerm unhandled case for: {str(term)} of type {type(term)}")
                rv = term
            assert rv is not None, f"{term} evaluated to None"
            return rv
        except Exception as e:
            contract_bug("Exception while evaluating " + str(term) + ". The exception: \n" + str(e) )

    def evalLit(self, litterm:Literal) -> Data:
        if isinstance(litterm, RoleIdLit):
            return litterm.lit
        elif isinstance(litterm, SimpleTimeDeltaLit):
            return litterm.lit
        else:
            return litterm.lit

    def evalTimeConstraint(self, time_constraint:Optional[Term]) -> bool:
        # print(f"evalTimeConstraint({time_constraint})",
        #       "cur event td", self.cur_event_delta(),
        #       "next event td", self.evalFnApp(FnApp("next_event_td",[],None)),
        #       "sit entrance td", self.evalFnApp(FnApp("last_situation_td", [], None)))
        if time_constraint is None:
            return True
        else:
            return chcast(bool, self.evalTerm(time_constraint))

    def evalFnApp(self,
                  fnapp:FnApp) -> Any:
        fn = fnapp.head
        evaluated_args = tuple(self.evalTerm(x) for x in fnapp.args)

        try:
            if fn in FN_SYMB_INTERP:
                return cast(Any,FN_SYMB_INTERP[fn])(*evaluated_args)
            elif fn in TIME_CONSTRAINT_OPERATORS or fn in TIME_CONSTRAINT_PREDICATES:
                assert self.cur_event is not None
                if fn == "monthStartDay_td":
                    dt = self.cur_event_datetime()
                    # use the datetime corresponding to start of month
                    month_start_dt = datetime(year=dt.year, month=dt.month, day=1)
                    return self.datetime2delta(month_start_dt)
                elif fn == "monthEndDay_td":
                    dt = self.cur_event_datetime()
                    # use the datetime corresponding to start of month
                    next_month = dt.month + 1 if dt.month < 12 else 1
                    year = dt.year if dt.month < 12 else dt.year + 1
                    day_after_month_end_dt = datetime(year=year, month=next_month, day=1)
                    last_day_of_month_dt = day_after_month_end_dt - timedelta(days=1)
                    return self.datetime2delta(last_day_of_month_dt)
                else:
                    contract_bug(f"Unhandled time constraint fn symbol: {fn}")
            elif fn in ENV_VAR_INTERP:
                assert fn != "future_event_td", "All occurrences of future_event_td should be eliminated before using interpreter_no_floating.py."

                if fn == "next_event_td":
                    assert self.evaluation_is_in_next_action_rule, "Can't use next_event_td when not in the scope of an action rule."

                # elif fn == "last_event_td":
                #     assert self.evaluation_is_in_action, "Can't use last_event_td when not in the scope of an action."
                #     assert not self.evaluation_is_in_next_action_rule, ("last_event_td directly within the time constraint or `where` clause of a next-action rule is not supported, because it's confusing." +
                #                                                       "Use last_situation_td instead.")
                # elif fn == "last_situation_td":
                #     assert self.evaluation_is_in_situation, "Can't use last_situation_td when not in the scope of a situation."

                return ENV_VAR_INTERP[fn](self)
            else:
                contract_bug(f"Unhandled fn symbol in evalFnApp: {fn}")
        except Exception as e:
            contract_bug(f"Error evaluating fn app {fnapp}:\n" + str(e))
            raise e

    def evalError(self, msg: str, thing:Any = None):
        raise EvalError(msg, thing)

    def assertOrEvalError(self, test:bool, msg:str, thing:Any = None):
        if not test:
            self.evalError(msg,thing) if thing else self.evalError(msg)

