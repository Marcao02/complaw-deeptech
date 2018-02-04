import logging
from datetime import datetime

from src.independent.util import hasNotNone, dictSetOrInc, todo_once, chcast, contract_bug, castid
from src.constants_and_defined_types import LOOP_KEYWORD, LocalVarSubst
from src.constants_and_defined_types import TIME_CONSTRAINT_OPERATORS, TIME_CONSTRAINT_PREDICATES, \
    ContractParamId, SectionId, ABAPSubst, \
    Data, GVarSubst, ContractParamSubst
from src.interpreter.interpreter_support import *
from src.model.Action import Action
from src.model.ActionRule import PartyNextActionRule, EnvNextActionRule, NextActionRule
from src.model.BoundVar import GlobalVar, ContractParam, ActionBoundActionParam, \
    RuleBoundActionParam, LocalVar
from src.model.ContractParamDec import ContractParamDec
from src.model.EventsAndTraces import Event, Trace, breachSectionId
from src.model.StateTransform import StateTransform
from src.model.Statement import IfElse, StateVarAssign, LocalVarDec
from src.model.StateVarDec import StateVarDec
from src.model.L4Contract import L4Contract
from src.model.Literal import Literal, DeadlineLit, SimpleTimeDeltaLit, RoleIdLit
from src.model.PartialEvalTerm import PartialEvalTerm
from src.model.Section import Section
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
        self.last_or_current_section_id: SectionId = prog.start_section_id
        self.last_appliedaction_params : Optional[ABAPSubst] = None
        # following changes only in evalTrace
        self.cur_event : Optional[Event] = None

        self.evaluation_is_in_action = False
        self.evaluation_is_in_section = False
        self.evaluation_is_in_next_action_rule = False

        # self.start_datetime = datetime.now(timezone.utc)
        # self.start_datetime : datetime = datetime(2000,1,1,0,0,0,0,timezone.utc)
        self.start_datetime: datetime = datetime(2000, 1, 1, 0, 0, 0, 0)
        self.absolute_timeint2timedelta_converter = self.getIntToDeltaConverter()
        self.last_section_entrance_delta : timedelta = timedelta(0) # = self.timeint2delta(0)

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
    def last_or_current_section(self) -> Section:
        return self.top.section(self.last_or_current_section_id)

    def evalTrace(self, trace:Trace, finalSectionId:Optional[SectionId] = None, final_var_vals: Optional[GVarSubst] = None, verbose=False, debug=False):
        prog = self.top
        self.evalContractParamDecs(prog.contract_params)
        self.evalGlobalVarDecs(prog.global_var_decs)

        for i in range(len(trace)):
            # print("Environ", self.environ_tostr())
            eventi = trace[i]
            self.cur_event = eventi
            cur_action_id = self.cur_event.action_id
            cur_event_delta = self.cur_event_delta()
            cur_event_datetime = self.delta2datetime(cur_event_delta)
            cur_action = self.top.action(cur_action_id)

            # print("last_section_entrance_delta", self.last_section_entrance_delta)

            if self.last_section_entrance_delta is not None and self.last_section_entrance_delta > cur_event_delta:
                self.evalError(f"Event timestamps must be non-decreasing, but last event timestamp "
                               f"{self.last_section_entrance_delta} is greater than current event timestamp {cur_event_delta}")

            if not cur_action: self.evalError(f"Don't recongize action id {cur_action_id}")

            if not self.top.section_mentions_action_in_nextaction_rule(self.last_or_current_section_id, cur_action_id):
                self.evalError(  f"Action type indicated using a next-rule, but current section {self.last_or_current_section_id} "
                                 f"has no such rule for action {cur_action_id}." )

            self.evaluation_is_in_section = True
            nextrule_assessment = self.assess_event_legal_wrt_nextrules(eventi)
            self.evaluation_is_in_section = False

            actionstr = event_to_action_str(eventi)
            srcid = self.last_or_current_section_id
            if isinstance(nextrule_assessment, EventOk):
                self.evaluation_is_in_action = True
                applyactionresult = self.apply_action(cur_action)
                self.evaluation_is_in_action = False
                if verbose:
                    print(f"[{cur_event_datetime}] {srcid} --{actionstr}--> {self.last_or_current_section_id}\n")

            elif isinstance(nextrule_assessment, BreachResult):
                print("Breach result:", nextrule_assessment)
                breach_section_id = breachSectionId(*nextrule_assessment.role_ids)
                self.apply_action(cur_action, to_breach_section_id = breach_section_id)

                if verbose:
                    print(f"[{cur_event_datetime}] {srcid} --{actionstr}--> {breach_section_id}")

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

        if final_var_vals:
            for gvarid,expected_val in final_var_vals.items():
                actual_val = self.gvarvals[castid(StateVarId, gvarid)]
                assert actual_val == expected_val, f"Expected global variable {gvarid} to have value {expected_val} at end of trace, but had value {actual_val}."

        if finalSectionId:
            assert self.last_or_current_section_id == finalSectionId, f"Trace expected to end in section {finalSectionId} but ended in section {self.last_or_current_section_id}"


    def assess_event_legal_wrt_nextrules(self, event:Event) -> EventLegalityAssessment:

        enabled_strong_obligs : List[PartyNextActionRule] = list()

        enabled_permissions : List[PartyNextActionRule] = list()
        enabled_env_action_rules : List[EnvNextActionRule] = list()

        enabled_weak_obligs_by_role : Dict[RoleId, List[PartyNextActionRule]] = dict()
        enabled_weak_obligs = list()

        nar: NextActionRule

        for nar in self.last_or_current_section.action_rules():
            entrance_enabled = not nar.entrance_enabled_guard or chcast(bool, self.evalTerm(nar.entrance_enabled_guard))

            if entrance_enabled:
                if isinstance(nar, EnvNextActionRule):
                    enabled_env_action_rules.append(nar)
                else:
                    assert isinstance(nar, PartyNextActionRule)
                    if nar.deontic_keyword == 'may' or nar.deontic_keyword == 'should':
                        enabled_permissions.append(nar)
                    elif nar.deontic_keyword == 'must':
                        enabled_strong_obligs.append(nar)
                    elif nar.deontic_keyword == 'obligation-options-include':
                        enabled_weak_obligs.append(nar)
                        if nar.role_id not in enabled_weak_obligs_by_role:
                            enabled_weak_obligs_by_role[nar.role_id] = [nar]
                        else:
                            enabled_weak_obligs_by_role[nar.role_id].append(nar)
                    else:
                        assert False


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
                assert isinstance(enabled_strong_obligs[0],PartyNextActionRule)
                if self.current_event_compatible_with_enabled_NextActionRule(enabled_strong_obligs[0]):
                    return EventOk()
                else:
                    return BreachResult({enabled_strong_obligs[0].role_id},
                                 f"The unique enabled strong obligation (of {enabled_strong_obligs[0].role_id}) is incompatible "
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
        compat_enabled_permissions = list(filter(self.current_event_compatible_with_enabled_NextActionRule, enabled_permissions))
        if len(compat_enabled_permissions) > 0:
            return EventOk()
        # Similarly for Env actions:
        compat_enabled_env_action_rules = list(filter(self.current_event_compatible_with_enabled_NextActionRule, enabled_env_action_rules))
        if len(compat_enabled_env_action_rules) > 0:
            return EventOk()

        # -----------------------------------------------------------------------------------
        # CASE 2b: the current event is NOT compatible with any permission or env action rule
        # -----------------------------------------------------------------------------------

        # Case 2b1: there are actually no enabled weak obligations. Thus the current event is not allowed,
        # so we blame the subject of the current event.
        if len(enabled_weak_obligs) == 0:
            return BreachResult([event.role_id],
                                f"Role {event.role_id} attempted an unpermitted action {event.action_id}({event.params})")
            # contract_bug(f"No rules apply to the current event\n{event}.\n"
            #              "This is a contract bug that eventually will be ruled out statically.")


        # Case 2b2: weak obligations are relevant even if they are not compatible with `event`,
        # since in that case they can result in a breach. Let's see who has weak obligations that match
        # the current event, by "filtering out" those that don't.
        # if there are any left, for and role, then all is well.
        for roleid_with_wo in enabled_weak_obligs_by_role:
            for rule in enabled_weak_obligs_by_role[roleid_with_wo]:
                # print(f"An enabled weak oblig rule for {roleid_with_wo}: " + str(rule))
                if self.current_event_compatible_with_enabled_NextActionRule(rule):
                    print("weak oblig rule checks out: ", rule)
                    return EventOk()
            # enabled_weak_obligs_by_role[roleid_with_wo] = list(filter( compat_checker, enabled_weak_obligs_by_role[roleid_with_wo] ))
            # if len(enabled_weak_obligs_by_role[roleid_with_wo]) > 0:
            #     return EventOk()

        # Case 2b3: all the roles (and there's at least one) who had an enabled weak oblig are jointly responsible for the breach
        breach_roles = list(enabled_weak_obligs_by_role.keys())
        return BreachResult(list(breach_roles),
                            f"Role(s) {list(breach_roles)} had weak obligations that went unfulfilled:\n" + str(enabled_weak_obligs_by_role))


    # Only if the current section is an anonymous section (i.e. given by a FollowingSection declaration) is it possible
    # for the where_clause of action_rule to contain action-bound action parameters.
    def current_event_compatible_with_enabled_NextActionRule(self, action_rule:NextActionRule)  -> bool:
        assert self.cur_event is not None
        rv : bool
        self.evaluation_is_in_next_action_rule = True
        role_action_match = self.cur_event.role_id == action_rule.role_id and self.cur_event.action_id == action_rule.action_id
        if not role_action_match:
            rv = False
        elif not self.evalTimeConstraint(action_rule.time_constraint):
            rv = False
        elif action_rule.where_clause:
            rv = chcast(bool,self.evalTerm(action_rule.where_clause))
        elif action_rule.fixed_args:
            if self.cur_event.params:
                argvals = [self.evalTerm(arg) for arg in action_rule.fixed_args]
                rv = all(argvals[i] == self.cur_event.params[i] for i in range(len(self.cur_event.params)))
            else:
                rv = len(action_rule.fixed_args) == 0
        else:
            rv = True
        self.evaluation_is_in_next_action_rule = False
        return rv

    def environ_tostr(self) -> str:
        return f"{str(self.gvarvals)}\n{str(self.last_appliedaction_params)}\n{str(self.contract_param_vals)}"

    def apply_action(self, action:Action, to_breach_section_id:Optional[SectionId] = None) -> ApplyActionResult:
        assert self.cur_event is not None

        if to_breach_section_id is not None:
            self.last_or_current_section_id = to_breach_section_id
            self.last_section_entrance_delta = self.cur_event_delta()
            return ApplyActionResult(None)

        self.last_appliedaction_params = self.cur_event.params
        rv: ApplyActionResult

        if action.global_state_transform:
            self.evalCodeBlock(action.global_state_transform)

        if action.dest_section_id != LOOP_KEYWORD:
            self.last_or_current_section_id = action.dest_section_id
        self.last_section_entrance_delta = self.cur_event_delta()

        return ApplyActionResult(None)


    def evalGlobalVarDecs(self, decs : Dict[StateVarId, StateVarDec]):
        for (var,dec) in decs.items():
            # print("dec: ", dec, dec.initval)
            if dec.initval is None:
                self.gvarvals[var] = None
                # print(f"Global var {var} has no initial value.")
                dictSetOrInc(self.gvar_write_cnt, var, init=0)
            else:
                self.gvarvals[var] = self.evalTerm(dec.initval)
                # print(f"Global var {var} has initial value {self.gvarvals[var]}.")
                dictSetOrInc(self.gvar_write_cnt, var, init=1)
            # print('evalGlobalVarDecs: ', var, dec, self.gvar_write_cnt[var])

    def evalContractParamDecs(self, decs : Dict[ContractParamId, ContractParamDec]):
        for (name,dec) in decs.items():
            # print("contract param dec", dec, type(dec.value_expr))
            if not(dec.value_expr is None):
                self.contract_param_vals[name] = self.evalTerm(dec.value_expr)
            else:
                self.contract_param_vals[name] = None
            # print(name, type(self.contract_param_vals[name]))


    def evalCodeBlock(self, transform:StateTransform):
        for statement in transform.statements:
            self.evalStatement(statement)

    def evalStatement(self, stmt:Statement):
        # An Action's StateTransform block is *always* evaluated in the most recent global variable and
        # action parameter substitution context that's visible to it, as given by self.gvarvals and self.cur_event,
        # even when applying an action from a PartlyInstantiatedPartyFutureActionRule.
        # Therefore, this function does not take an EvalContext argument.

        if isinstance(stmt, StateVarAssign):
            gvardec = stmt.vardec
            assert gvardec is not None, f"Global variable declaration for {stmt.varname} not found."

            if gvardec.isWriteOnceMore() and self.gvar_write_cnt[stmt.varname] >= 2:
                raise Exception(f"Attempt to write twice more (after init) to writeOnceMore variable `{stmt.varname}`")
            dictSetOrInc(self.gvar_write_cnt, stmt.varname, init=1)
            todo_once("use vardec for runtime type checking")
            rhs_value = self.evalTerm(stmt.value_expr)

            if stmt.varop == ":=":
                self.gvarvals[stmt.varname] = rhs_value
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
                self.evalCodeBlock(StateTransform(stmt.true_branch))
            elif stmt.false_branch:
                self.evalCodeBlock(StateTransform(stmt.false_branch))

        elif isinstance(stmt, LocalVarDec):
            rhs_value = self.evalTerm(stmt.value_expr)
            self.localvar_vals[stmt.varname] = rhs_value

        else:
            raise NotImplementedError("Unhandled Statement: " + str(stmt))


    def evalTerm(self,
                 term:Term) -> Any:
        assert term is not None
        # assert self.cur_event is not None  # nope! it can be None when evaluating terms in contract param dec and global var decs

        # next_event_timestamp will be None when evaluating an entrance-guard
        try:
            if isinstance(term, FnApp):
                return self.evalFnApp(term)

            elif isinstance(term, Literal):
                return self.evalLit(term)

            elif isinstance(term, GlobalVar):
                return self.gvarvals[term.name]

            elif isinstance(term, ContractParam):
                assert hasNotNone(self.contract_param_vals, term.name), term.name
                return self.contract_param_vals[term.name]

            elif isinstance(term, ActionBoundActionParam):
                assert self.last_appliedaction_params is not None
                return self.last_appliedaction_params[term.ind]

            elif isinstance(term, RuleBoundActionParam):
                # assert hasNotNone(self.cur_event.params_by_abap_name, term.name), f"Trying to get subst value of an RuleBoundActionParam {term.name} but didn't find it among the action parameters."
                assert self.cur_event and self.cur_event.params is not None, f"Expected current event {self.cur_event} to have an action parameter named {term}"
                return self.cur_event.params[term.ind]

            elif isinstance(term, PartialEvalTerm):
                self.evalError("There should be no PartialEvalTerm in this version of ExecEnv.")

            elif isinstance(term, SimpleTimeDeltaLit):
                return term.lit

            elif isinstance(term, LocalVar):
                return self.localvar_vals[term.name]

            else:
                contract_bug(f"evalTerm unhandled case for: {str(term)} of type {type(term)}")
                return term

        except Exception as e:
            contract_bug("Exception while evaluating " + str(term) + ". The exception: \n" + str(e) )

    def evalLit(self, litterm:Literal) -> Data:
        if isinstance(litterm, DeadlineLit):
            todo_once("DeadlineLit not correctly handled yet")
            # print("DeadlineLit: ", term, next_event_timestamp == self.last_section_entrance_delta)
            # return self.cur_event.timestamp == self.last_section_entrance_delta
            return True
        elif isinstance(litterm, RoleIdLit):
            return litterm.lit
        elif isinstance(litterm, SimpleTimeDeltaLit):
            return litterm.lit
        else:
            return litterm.lit

    def evalTimeConstraint(self, time_constraint:Term) -> bool:
        assert time_constraint is not None
        rv = chcast(bool, self.evalTerm(time_constraint))
        return rv

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
                if fn == "monthEndDay_td":
                    dt = self.cur_event_datetime()
                    # use the datetime corresponding to start of month
                    next_month = dt.month + 1 if dt.month < 12 else 1
                    year = dt.year if dt.month < 12 else dt.year + 1
                    day_after_month_end_dt = datetime(year=year, month=next_month, day=1)
                    last_day_of_month_dt = day_after_month_end_dt - timedelta(days=1)
                    return self.datetime2delta(last_day_of_month_dt)
                else:
                    contract_bug(f"Unhandled time constraintfn symbol: {fn}")
            elif fn in ENV_VAR_INTERP:
                todo_once("Statically check ENV_VAR_INTERP scope.")
                assert fn != "future_event_td", "All occurrences of future_event_td should be eliminated before using interpreter_no_floating.py."

                if fn == "next_event_td":
                    assert self.evaluation_is_in_next_action_rule, "Can't use event_td when not in the scope of an action rule."
                elif fn == "event_td":
                    assert self.evaluation_is_in_action, "Can't use event_td when not in the scope of an action."
                    assert not self.evaluation_is_in_next_action_rule, ("event_td directly within the time constraint or `where` clause of a next-action rule is not currently supported." +
                                                                      "Evaluate it in the global state transform and save it to a local variable.")
                elif fn == "sectionEntrance_td":
                    assert self.evaluation_is_in_section, "Can't use sectionEntrance_td when not in the scope of a section."

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

