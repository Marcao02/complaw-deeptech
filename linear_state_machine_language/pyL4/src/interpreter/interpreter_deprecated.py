import logging
from datetime import datetime

from src.independent.util import hasNotNone, dictSetOrInc, todo_once, chcast, contract_bug, castid
from src.constants_and_defined_types import LOOP_KEYWORD, LocalVarSubst
from src.constants_and_defined_types import TIME_CONSTRAINT_OPERATORS, TIME_CONSTRAINT_PREDICATES, \
    ContractParamId, SectionId, ABAPSubst, \
    Data, GVarSubst, ContractParamSubst
from src.interpreter.interpreter_support import *
from src.model.Action import Action
from src.model.ActionRule import PartyNextActionRule, EnvNextActionRule, NextActionRule, \
    PartlyInstantiatedPartyFutureActionRule
from src.model.BoundVar import GlobalVar, ContractParam, ActionBoundActionParam, \
    RuleBoundActionParam, StateTransformLocalVar
from src.model.ContractParamDec import ContractParamDec
from src.model.EvalContext import EvalContext
from src.model.EventsAndTraces import Event, Trace, breachSectionId, EventType
from src.model.GlobalStateTransform import GlobalStateTransform
from src.model.GlobalStateTransformStatement import IfElse, GlobalVarAssignStatement, StateTransformLocalVarDec
from src.model.GlobalVarDec import GlobalVarDec
from src.model.L4Contract import L4Contract
from src.model.Literal import Literal, DeadlineLit, SimpleTimeDeltaLit
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
        self.gvar_write_cnt : Dict[GlobalVarId, int] = dict()

        # following 2 change only in apply_action:
        self.last_or_current_section_id: SectionId = prog.start_section_id
        self.last_appliedaction_params : Optional[ABAPSubst] = None
        # following changes only in evalTrace
        self.cur_event : Optional[Event] = None

        # self.start_datetime = datetime.now(timezone.utc)
        # self.start_datetime : datetime = datetime(2000,1,1,0,0,0,0,timezone.utc)
        self.start_datetime: datetime = datetime(2000, 1, 1, 0, 0, 0, 0)
        self.absolute_timeint2timedelta_converter = self.getIntToDeltaConverter()
        self.last_section_entrance_delta : timedelta = timedelta(0) # = self.timeint2delta(0)

        self.future_permissions : List[PartlyInstantiatedPartyFutureActionRule] = []
        self.future_obligations : List[PartlyInstantiatedPartyFutureActionRule] = []


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


    def futures(self) -> Iterator[PartlyInstantiatedPartyFutureActionRule]:
        return chain(self.future_obligations, self.future_permissions)

    def delete_future(self, future:PartlyInstantiatedPartyFutureActionRule) -> bool:
        assert future not in self.future_permissions or future not in self.future_obligations
        if future in self.future_obligations:
            self.future_obligations.remove(future)
            return True
        elif future in self.future_permissions:
            self.future_permissions.remove(future)
            return True

        return False

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


            if eventi.type == EventType.fulfill_floating_obligation or eventi.type == EventType.use_floating_permission:
                floatingrule_to_apply : Optional[PartlyInstantiatedPartyFutureActionRule] = None
                for pifar in self.futures():
                    # print("evaluating", str(pifar), "in event context", str(self.cur_event))
                    if self.current_event_compatible_with_floatingrule(pifar):
                        assert floatingrule_to_apply is None, ("There are at least two floating rules that seem to apply:\n" +
                                                               str(floatingrule_to_apply) + "\n" + str(pifar))
                        floatingrule_to_apply = pifar
                if not floatingrule_to_apply:
                    if verbose:
                        actionstr = event_to_action_str(eventi)
                        print(f"[{cur_event_datetime}] {self.last_or_current_section_id} --{actionstr}--> {breachSectionId(eventi.role_id)}")

                else:
                    assert floatingrule_to_apply is not None, 'Event type indicated a floating permission or obligation, but no compatible such rule was found.'
                    self.delete_future(floatingrule_to_apply)
                    prev_section_id = self.last_or_current_section_id
                    applyactionresult = self.apply_action(cur_action)
                    assert prev_section_id == self.last_or_current_section_id, "An action fulfilling a floating obligation " \
                            "or using a floating permission is not allowed to change the current Section of the contract."
                    if verbose:
                        actionstr = event_to_action_str(eventi)
                        frules_added_str = f' and added {applyactionresult.floatingrules_added}' if applyactionresult.floatingrules_added else ''
                        frule_deleted_str = f' and deleted [{floatingrule_to_apply}]' if floatingrule_to_apply else ''
                        print(f"[{cur_event_datetime}] {prev_section_id} --{actionstr}--> {self.last_or_current_section_id} {frule_deleted_str} {frules_added_str}")

            else:
                if not self.top.section_mentions_action_in_nextaction_rule(self.last_or_current_section_id, cur_action_id):
                    self.evalError(  f"Action type indicated using a next-rule, but current section {self.last_or_current_section_id} "
                                     f"has no such rule for action {cur_action_id}." )

                nextrule_assessment = self.assess_event_legal_wrt_nextrules(eventi)
                actionstr = event_to_action_str(eventi)
                srcid = self.last_or_current_section_id
                if isinstance(nextrule_assessment, EventOk):

                    applyactionresult = self.apply_action(cur_action)

                    if verbose:
                        frules_added_str = f' and added {applyactionresult.floatingrules_added}' if applyactionresult.floatingrules_added else ''
                        print(f"[{cur_event_datetime}] {srcid} --{actionstr}--> {self.last_or_current_section_id} {frules_added_str}")

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
                from ipdb import set_trace # type: ignore
                set_trace()  # type: ignore
                # other things tried:
                # code.InteractiveConsole(locals=locals()).interact()
                # IPython.embed()

        if final_var_vals:
            for gvarid,expected_val in final_var_vals.items():
                actual_val = self.gvarvals[castid(GlobalVarId,gvarid)]
                assert actual_val == expected_val, f"Expected global variable {gvarid} to have value {expected_val} at end of trace, but had value {actual_val}."

        if len(self.future_obligations) > 0:
            roles = set()
            for o in self.future_obligations:
                roles.add(o.rule.role_id)
            self.last_or_current_section_id = breachSectionId(*list(roles))
            print("Trace ended with obligations remaining (Which is ok; finalSectionId, if provided, will need to be the proper breach section)")
            # assert False,
            # return

        if finalSectionId:
            assert self.last_or_current_section_id == finalSectionId, f"Trace expected to end in section {finalSectionId} but ended in section {self.last_or_current_section_id}"


    def assess_event_legal_wrt_nextrules(self, event:Event) -> EventLegalityAssessment:

        enabled_strong_obligs : List[PartyNextActionRule] = list()

        enabled_permissions : List[PartyNextActionRule] = list()
        enabled_env_action_rules : List[EnvNextActionRule] = list()

        enabled_weak_obligs_by_role : Dict[RoleId, List[PartyNextActionRule]] = dict()
        enabled_weak_obligs = list()

        nar: NextActionRule
        ctx = EvalContext(self.gvarvals,
                          self.last_appliedaction_params if self.last_or_current_section.is_anon() else None,
                          use_current_event_params_for_rulebound_action_params = False)

        compat_checker = lambda x: self.current_event_compatible_with_enabled_NextActionRule(x,ctx)

        for nar in self.last_or_current_section.action_rules():
            entrance_enabled = not nar.entrance_enabled_guard or chcast(bool, self.evalTerm(nar.entrance_enabled_guard, ctx))

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
                if compat_checker(enabled_strong_obligs[0]):
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
        compat_enabled_permissions = list(filter(compat_checker, enabled_permissions))
        if len(compat_enabled_permissions) > 0:
            return EventOk()
        # Similarly for Env actions:
        compat_enabled_env_action_rules = list(filter(compat_checker, enabled_env_action_rules))
        if len(compat_enabled_env_action_rules) > 0:
            return EventOk()

        # -----------------------------------------------------------------------------------
        # CASE 2b: the current event is NOT compatible with any permission or env action rule
        # -----------------------------------------------------------------------------------

        # Case 2b1: there are actually no enabled weak obligations. Thus the current event is not allowed,
        # so we blame the subject of the current event.
        if len(enabled_weak_obligs) == 0:
            return BreachResult([event.role_id],
                                f"Role {event.role_id} attempted an unpermitted action.")
            # contract_bug(f"No rules apply to the current event\n{event}.\n"
            #              "This is a contract bug that eventually will be ruled out statically.")


        # Case 2b2: weak obligations are relevant even if they are not compatible with `event`,
        # since in that case they can result in a breach. Let's see who has weak obligations that match
        # the current event, by "filtering out" those that don't.
        # if there are any left, for and role, then all is well.
        for roleid_with_wo in enabled_weak_obligs_by_role:
            for rule in enabled_weak_obligs_by_role[roleid_with_wo]:
                print(f"An enabled weak oblig rule for {roleid_with_wo}: " + str(rule))
                if compat_checker(rule):
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
    def current_event_compatible_with_enabled_NextActionRule(self, action_rule:NextActionRule, ctx:EvalContext)  -> bool:
        assert self.cur_event is not None
        role_action_match = self.cur_event.role_id == action_rule.role_id and self.cur_event.action_id == action_rule.action_id
        if not role_action_match:
            return False

        # if action_rule.action_id == "Deliver":
        #     print("Looking at Deliver rule")
        #     print(f"event_td is {self.cur_event_delta()}")
        #     print("time constraint of rule: ", str(action_rule.time_constraint))

        if not self.evalTimeConstraint(action_rule.time_constraint, ctx):
            return False

        if action_rule.where_clause:
            return chcast(bool,self.evalTerm(action_rule.where_clause, None))
        elif action_rule.fixed_args:
            if self.cur_event.params:
                argvals = [self.evalTerm(arg,None) for arg in action_rule.fixed_args]
                return all(argvals[i] == self.cur_event.params[i] for i in range(len(self.cur_event.params)))
            else:
                return len(action_rule.fixed_args) == 0
        else:
            return True


    def current_event_compatible_with_floatingrule(self, pif_action_rule: PartlyInstantiatedPartyFutureActionRule) -> bool:
        assert self.cur_event is not None
        role_action_match = self.cur_event.role_id == pif_action_rule.rule.role_id and self.cur_event.action_id == pif_action_rule.rule.action_id
        if not role_action_match: return False

        # will sub in self.cur_event.params for *rule*-bound action params in pif_action_rule.pe_where_clause
        # all other variables value in pif_action_rule.pe_where_clause are supplied by pif_action_rule.ctx
        # note that whether the current event is compatible with a future is not allowed to depend on
        # anything from the current execution context except self.cur_event.params. that is the reason for the None
        # argument.

        # TODO UNCERTAIN

        if pif_action_rule.pe_where_clause:
            ctx = EvalContext(self.gvarvals, None, use_current_event_params_for_rulebound_action_params=True)
            return chcast(bool,self.evalTerm(pif_action_rule.pe_where_clause, ctx))
        elif pif_action_rule.fixed_param_vals:
            if self.cur_event.params:
                return all(pif_action_rule.fixed_param_vals[i] == self.cur_event.params[i] for i in range(len(self.cur_event.params)))
            else:
                return len(pif_action_rule.fixed_param_vals) == 0
        else:
            return True


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

        future : PartlyInstantiatedPartyFutureActionRule
        floatingrules_added : List[PartlyInstantiatedPartyFutureActionRule] = []
        for far in action.futures:
            is_disabled = far.entrance_enabled_guard and \
                          not chcast(bool,self.evalTerm(far.entrance_enabled_guard,None)) # TODO: unsure about this None
            if is_disabled:
                continue

            if far.where_clause:
                new_where_clause = PartialEvalTerm(
                    far.where_clause,
                    EvalContext(self.gvarvals.copy(), self.last_appliedaction_params.copy() if self.last_appliedaction_params else None, True)
                )
                future = PartlyInstantiatedPartyFutureActionRule(far, new_where_clause, None)
            elif far.fixed_args:
                future = PartlyInstantiatedPartyFutureActionRule(far, None, [self.evalTerm(arg,None) for arg in far.fixed_args])
            else:
                future = PartlyInstantiatedPartyFutureActionRule(far, None, None)

            if far.deontic_keyword == 'must-later':
                self.future_obligations.append(future)
            else:
                assert  far.deontic_keyword == 'may-later'
                self.future_permissions.append(future)
            floatingrules_added.append(future)
            # print("new future:", ef)

        if action.dest_section_id != LOOP_KEYWORD:
            self.last_or_current_section_id = action.dest_section_id
        self.last_section_entrance_delta = self.cur_event_delta()

        return ApplyActionResult(floatingrules_added if len(floatingrules_added) > 0 else None)


    def evalGlobalVarDecs(self, decs : Dict[GlobalVarId, GlobalVarDec]):
        for (var,dec) in decs.items():
            # print("dec: ", dec, dec.initval)
            if dec.initval is None:
                self.gvarvals[var] = None
                # print(f"Global var {var} has no initial value.")
                dictSetOrInc(self.gvar_write_cnt, var, init=0)
            else:
                self.gvarvals[var] = self.evalTerm(dec.initval, None)
                # print(f"Global var {var} has initial value {self.gvarvals[var]}.")
                dictSetOrInc(self.gvar_write_cnt, var, init=1)
            # print('evalGlobalVarDecs: ', var, dec, self.gvar_write_cnt[var])

    def evalContractParamDecs(self, decs : Dict[ContractParamId, ContractParamDec]):
        for (name,dec) in decs.items():
            # print("contract param dec", dec, type(dec.value_expr))
            if not(dec.value_expr is None):
                self.contract_param_vals[name] = self.evalTerm(dec.value_expr, None)
            else:
                self.contract_param_vals[name] = None
            # print(name, type(self.contract_param_vals[name]))


    def evalCodeBlock(self, transform:GlobalStateTransform):
        for statement in transform.statements:
            self.evalStatement(statement)

    def evalStatement(self, stmt:GlobalStateTransformStatement):
        # An Action's GlobalStateTransform block is *always* evaluated in the most recent global variable and
        # action parameter substitution context that's visible to it, as given by self.gvarvals and self.cur_event,
        # even when applying an action from a PartlyInstantiatedPartyFutureActionRule.
        # Therefore, this function does not take an EvalContext argument.

        if isinstance(stmt, (GlobalVarAssignStatement, IncrementStatement, DecrementStatement, TimesEqualsStatement)):
            gvardec = self.top.gvarDecObj(stmt.varname)
            assert gvardec is not None, f"Global variable declaration for {stmt.varname} not found."

            if gvardec.isWriteOnceMore() and self.gvar_write_cnt[stmt.varname] >= 2:
                raise Exception(f"Attempt to write twice more (after init) to writeOnceMore variable `{stmt.varname}`")
            dictSetOrInc(self.gvar_write_cnt, stmt.varname, init=1)
            todo_once("use vardec for runtime type checking")
            rhs_value = self.evalTerm(stmt.value_expr, None)

        if isinstance(stmt, GlobalVarAssignStatement):
            self.gvarvals[stmt.varname] = rhs_value
            print(f"\t{stmt.varname} := {rhs_value}")

        elif isinstance(stmt, (IncrementStatement,DecrementStatement,TimesEqualsStatement)):
            current_var_val : Data = self.gvarvals[stmt.varname]
            assert current_var_val is not None

            if isinstance(stmt, IncrementStatement):
                self.gvarvals[stmt.varname] = current_var_val + rhs_value
                print(f"\t{stmt.varname} := {self.gvarvals[stmt.varname]}")
            elif isinstance(stmt, DecrementStatement):
                self.gvarvals[stmt.varname] = current_var_val - rhs_value
                print(f"\t{stmt.varname} := {self.gvarvals[stmt.varname]}")
            elif isinstance(stmt, TimesEqualsStatement):
                self.gvarvals[stmt.varname] = current_var_val * rhs_value
                print(f"\t{stmt.varname} := {self.gvarvals[stmt.varname]}")
            else: raise Exception('fixme')


        elif isinstance(stmt, InCodeConjectureStatement):
            # print("conjecture " + str(stmt))
            assert self.evalTerm(stmt.value_expr, None), f"""Conjecture {stmt.value_expr} is false! Variable values and action params:
            {str(self.gvarvals)}
            {str(self.last_appliedaction_params)}
            """

        elif isinstance(stmt, IfElse):
            test_result = chcast(bool, self.evalTerm(stmt.test, None))
            if test_result:
                self.evalCodeBlock(GlobalStateTransform( stmt.true_branch ))
            elif stmt.false_branch:
                self.evalCodeBlock(GlobalStateTransform(stmt.false_branch))

        elif isinstance(stmt, StateTransformLocalVarDec):
            rhs_value = self.evalTerm(stmt.value_expr, None)
            self.localvar_vals[stmt.varname] = rhs_value

        else:
            raise NotImplementedError("Unhandled GlobalStateTransformStatement: " + str(stmt))


    def evalTerm(self,
                 term:Term,
                 ctx:Optional[EvalContext]) -> Any:
        assert term is not None
        # assert self.cur_event is not None  # nope! it can be None when evaluating terms in contract param dec and global var decs

        # if partialeval_globals_subst:
        #     print("partialeval_globals_subst is not None. `term` is ", term)
        # next_event_timestamp will be None when evaluating an entrance-guard
        # print('evalTerm: ', term)
        try:
            if isinstance(term, FnApp):
                return self.evalFnApp(term, ctx)

            elif isinstance(term, Literal):
                return self.evalLit(term)

            elif isinstance(term, GlobalVar):
                # print(self.contract_param_vals)
                # print(self.gvarvals)
                if ctx:
                    assert hasNotNone(ctx.gvarvals, term.name), "Global var " + term.name + " should have a value but doesn't."
                    return ctx.gvarvals[term.name]
                else:
                    return self.gvarvals[term.name]

            elif isinstance(term, ContractParam):
                # print("ContractParam case of evalTerm: ", term)
                # print(self.contract_param_vals[term.name], type(self.contract_param_vals[term.name]))
                assert hasNotNone(self.contract_param_vals, term.name), term.name
                if term.name == "CONTRACT_LIFE":
                    print("...", self.contract_param_vals[term.name])

                return self.contract_param_vals[term.name]

            elif isinstance(term, ActionBoundActionParam):
                # if term.name == 'amount':
                #     print("Looking at 'amount', this is self.last_appliedaction_params:", self.last_appliedaction_params)
                # print('ActionBoundActionParam:', term)
                if ctx and ctx.abapvals:
                    # print(f"ActionBoundActionParam value for {term.name} taken from ctx")
                    # assert hasNotNone(ctx.,term.name), f"Trying to get subst value of an ActionBoundActionParam {term.name} but didn't find it in the execution context."
                    return ctx.abapvals[term.ind]
                else:
                    assert self.last_appliedaction_params is not None
                    return self.last_appliedaction_params[term.ind]

            elif isinstance(term, RuleBoundActionParam):
                # assert hasNotNone(self.cur_event.params_by_abap_name, term.name), f"Trying to get subst value of an RuleBoundActionParam {term.name} but didn't find it among the action parameters."
                assert self.cur_event and self.cur_event.params is not None, f"Expected current event {self.cur_event} to have an action parameter named {term}"
                return self.cur_event.params[term.ind]

                # return ctx.rbapvals[term.name]

            elif isinstance(term, PartialEvalTerm):
                # assert not partialeval_globals_subst, "haven't handled partial eval of partial eval yet"
                # assert not partialeval_actionparam_subst, "haven't handled partial eval of partial eval yet"
                # print("Current ctx:", ctx)
                # print("PartialEvalTerm's ctx:", term.ctx)
                assert not ctx or ctx.abapvals is None or len(ctx.abapvals) == 0, "TODO: ctx merge"

                return self.evalTerm(term.term, term.ctx)

            elif isinstance(term, SimpleTimeDeltaLit):
                return term.timedelta

            elif isinstance(term, StateTransformLocalVar):
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
        elif isinstance(litterm, SimpleTimeDeltaLit):
            return litterm.timedelta
        else:
            return litterm.lit

    def evalTimeConstraint(self, time_constraint:Term, ctx:EvalContext) -> bool:
        assert time_constraint is not None
        rv = chcast(bool, self.evalTerm(time_constraint, ctx))
        # print('evalTimeConstraint: ', rv)
        return rv

    def evalFnApp(self,
                  fnapp:FnApp,
                  ctx:Optional[EvalContext]) -> Any:
        fn = fnapp.head
        evaluated_args = tuple(self.evalTerm(x,ctx) for x in fnapp.args)

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

