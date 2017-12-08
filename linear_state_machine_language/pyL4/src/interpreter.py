import logging
from datetime import datetime, timedelta, tzinfo, timezone
from itertools import chain
import math
import copy

from src.model.constants_and_defined_types import TimeInt, LOOP_KEYWORD
from src.model.EvalContext import EvalContext
from src.model.Action import Action
from src.model.BoundVar import BoundVar, GlobalVar, ContractParam, ActionBoundActionParam, \
    RuleBoundActionParam
from src.model.ContractParamDec import ContractParamDec
from src.model.EventsAndTraces import Event, Trace, CompleteTrace, breachSectionId, EventType
from src.model.GlobalStateTransform import GlobalStateTransform
from src.model.GlobalStateTransformStatement import *
from src.model.GlobalVarDec import GlobalVarDec
from src.model.Literal import StringLit, Literal, DeadlineLit, SimpleTimeDeltaLit
from src.model.PartialEvalTerm import PartialEvalTerm
from src.model.Term import FnApp

logging.basicConfig(
    format="[%(levelname)s] %(funcName)s: %(message)s",
    level=logging.INFO )
from typing import Tuple, Any, cast, Dict, Iterable, Iterator, Sequence, NewType, Callable

from src.model.L4Contract import L4Contract

from src.model.ActionRule import PartyNextActionRule, EnvNextActionRule, NextActionRule, PartyFutureActionRule, \
    ActionRule, PartlyInstantiatedPartyFutureActionRule
from src.model.Section import Section
from src.sexpr_to_L4Contract import L4ContractConstructor
from src.parse_sexpr import prettySExprStr, parse_file
from src.model.constants_and_defined_types import GlobalVarId, ActionId, DEADLINE_OPERATORS, DEADLINE_PREDICATES, \
    RoleId, \
    RuleBoundActionParamId, ContractParamId, SectionId, ActionBoundActionParamId, FULFILLED_SECTION_LABEL, ABAPSubst, \
    Data, ENV_ROLE, GVarSubst, ContractParamSubst, ABAPNamedSubst
from src.model.util import hasNotNone, dictSetOrInc, todo_once, chcast, contract_bug, mapjoin

feedback = logging


class ApplyActionResult(NamedTuple):
    floatingrules_added: Optional[List[PartlyInstantiatedPartyFutureActionRule]]

class EventLegalityAssessment:
    pass

class BreachResult(EventLegalityAssessment):
    def __init__(self, role_ids:Iterable[RoleId], msg:str) -> None:
        self.role_ids = role_ids
        self.msg = msg
    def __str__(self) -> str:
        return f"{list(self.role_ids)}, {self.msg}"

class ContractFlawedError(EventLegalityAssessment):
    pass

class EventOk(EventLegalityAssessment):
    pass


FN_SYMB_INTERP = {
    # '+': lambda *args: sum(args),  # doesn't work with timedelta
    '+': lambda x,y: x + y,  # doesn't work with timedelta
    '-': lambda x,y: x - y,
    '/': lambda x,y: x / y,
    '*': lambda x,y: x * y,
    'even': lambda x: x % 2 == 0,
    'odd': lambda x: x % 2 == 1,
    'min' : min,
    'max' : max,
    '==': lambda x,y: x == y,
    '≤': lambda x,y: x <= y,
    '≥': lambda x,y: x >= y,
    '<': lambda x,y: x < y,
    '>': lambda x,y: x > y,
    'and': lambda x,y: x and y,
    'and*': all,
    'or': lambda x,y: x or y,
    'unitsAfter' : lambda x,y: x + y,
    'days': lambda x: timedelta(days=x),
    'round': round,
    'ceil': math.ceil
}

DEADLINE_OP_INTERP = {
    'event_ts': lambda entrance_ts, event_ts, args_ts: event_ts,
    'by': lambda entrance_ts, event_ts, args_ts: (event_ts <= args_ts[0]),

    'nonstrictly-within': lambda entrance_ts, event_ts, args_dur: (event_ts <= entrance_ts + args_dur[0]),
    'strictly-within': lambda entrance_ts, event_ts, args_dur: (event_ts < entrance_ts + args_dur[0]),

    'nonstrictly-before': lambda entrance_ts, event_ts, args_t: (event_ts <= args_t[0]),
    'strictly-before': lambda entrance_ts, event_ts, args_t: (event_ts < args_t[0]),
    
    'nonstrictly-after-ts-and-within': lambda entrance_ts, event_ts, args: args[0] <= event_ts <= entrance_ts + args[1],
    
    'at-ts': lambda entrance_ts, event_ts, args_ts: (event_ts == args_ts[0]),
    'immediately-after-ts': lambda entrance_ts, event_ts, args_ts: (event_ts == args_ts[0] + 1),
    'after-exact-duration': lambda entrance_ts, event_ts, args_dur: (event_ts == entrance_ts + args_dur[0]),
    'strictly-after-ts': lambda entrance_ts, event_ts, args_ts: (event_ts > args_ts[0]), # ??
    'nonstrictly-after-ts': lambda entrance_ts, event_ts, args_ts: (event_ts >= args_ts[0]),
}

def event_to_action_str(event:Event):
    if event.params_by_abap_name:
        params_str = ", ".join([f"{key}: {event.params_by_abap_name[key]}" for key in event.params_by_abap_name.keys()])
        return f"{event.action_id}({params_str})"
    else:
        return f"{event.action_id}"

class EvalError(Exception):
    pass

class ExecEnv:
    # All the instance variables of ExecEnv should probably be prefixed with _ to indicate not to use access them
    # outside of the ExecEnv class.
    # But I don't actually care if you access them outside the ExecEnv class. It's not that kind of class.
    # So, I'm prefixing none of them.

    def __init__(self, prog:L4Contract) -> None:
        self.top : L4Contract = prog
        self.contract_param_vals: ContractParamSubst = dict()

        self.gvarvals: GVarSubst = dict()
        self.gvar_write_cnt : Dict[GlobalVarId, int] = dict()

        # following 3 change only in apply_action:
        self.last_or_current_section_id: SectionId = prog.start_section_id
        self.last_appliedaction_params : Optional[ABAPSubst] = None
        # following changes only in evalTrace
        self.cur_event : Optional[Event] = None

        # self.start_datetime = datetime.now(timezone.utc)
        self.start_datetime : datetime = datetime(2000,1,1,0,0,0,0,timezone.utc)
        self.absolute_timeint2timedelta_converter = self.getTimeintConverter()
        self.last_section_entrance_timestamp = self.toTimeDelta(0)
        # self.duration_converter = ExecEnv.select_duration_converter()

        self.future_permissions : List[PartlyInstantiatedPartyFutureActionRule] = []
        self.future_obligations : List[PartlyInstantiatedPartyFutureActionRule] = []


    def getTimeintConverter(self) -> Callable[[int],timedelta]:
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

    def datetime2delta(self, dt:datetime) -> timedelta:
        return dt - self.start_datetime

    def delta2datetime(self, td:timedelta) -> datetime:
        return self.start_datetime + td

    def toTimeDelta(self, x:int) -> timedelta:
        return self.absolute_timeint2timedelta_converter(x)

    def cur_event_datetime(self) -> datetime:
        assert self.cur_event is not None
        return self.delta2datetime(self.toTimeDelta(self.cur_event.timestamp))

    # def toSimpleTimeDelta(self,x:int) -> SimpleTimeDeltaLit:
    #     return SimpleTimeDeltaLit(x, self.top.timeunit)

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

    def evalTrace(self, trace:Trace, finalSectionId:Optional[SectionId] = None, final_var_vals: Optional[GVarSubst] = None, verbose=False):
        prog = self.top
        self.evalContractParamDecs(prog.contract_params)
        self.evalGlobalVarDecs(prog.global_var_decs)

        # max_section_id_len = max(max(
        #     len(self.top.action(event.action_id).dest_section_id) for event in trace
        # ), len(finalSectionId) if finalSectionId else 0)
        # max_action_str_len = max(
        #     len(event_to_action_str(event)) for event in trace
        # )
        # sec_pad = ' ' * (max_section_id_len - len(self.last_or_current_section_id))
        # # act_pad = ' '*(max_action_str_len-len(actionstr))
        # act_pad = ''

        for i in range(len(trace)):
            print("Environ", self.environ_tostr())
            eventi = trace[i]
            self.cur_event = eventi
            cur_action_id, cur_event_timestamp = self.cur_event.action_id, self.toTimeDelta(self.cur_event.timestamp)
            cur_action = self.top.action(cur_action_id)

            # print("last_section_entrance_timestamp", self.last_section_entrance_timestamp)

            if self.last_section_entrance_timestamp is not None and self.last_section_entrance_timestamp > cur_event_timestamp:
                self.evalError( f"Event timestamps must be non-decreasing, but last event timestamp "
                                f"{self.last_section_entrance_timestamp} is greater than current event timestamp {cur_event_timestamp}")

            if not cur_action: self.evalError(f"Don't recongize action id {cur_action_id}")


            if eventi.type == EventType.fulfill_floating_obligation or eventi.type == EventType.use_floating_permission:
                floatingrule_to_apply : Optional[PartlyInstantiatedPartyFutureActionRule] = None
                for pifar in self.futures():
                    # print("evaluating", str(pifar), "in event context", str(self.cur_event))
                    if self.current_event_compatible_with_floatingrule(pifar):
                        # print("evaluated to True")
                        assert floatingrule_to_apply is None, ("There are at least two floating rules that seem to apply:\n" +
                                                               str(floatingrule_to_apply) + "\n" + str(pifar))
                        floatingrule_to_apply = pifar
                assert floatingrule_to_apply is not None, 'Event type indicated a floating permission or obligation, but no compatible such rule was found.'
                self.delete_future(floatingrule_to_apply)
                prev_section_id = self.last_or_current_section_id
                applyactionresult = self.apply_action(cur_action)
                assert prev_section_id == self.last_or_current_section_id, "An action is fulfilling a floating obligation " \
                        "or using a floating permission is not allowed to change the current Section of the contract."
                if verbose:
                    actionstr = event_to_action_str(eventi)
                    frules_added_str = f' and added {applyactionresult.floatingrules_added}' if applyactionresult.floatingrules_added else ''
                    frule_deleted_str = f' and deleted [{floatingrule_to_apply}]' if floatingrule_to_apply else ''
                    print(f"{prev_section_id} --{actionstr}--> {self.last_or_current_section_id} {frule_deleted_str} {frules_added_str}")

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
                        print(f"{srcid} --{actionstr}--> {self.last_or_current_section_id} {frules_added_str}")

                elif isinstance(nextrule_assessment, BreachResult):
                    print("Breach result:", nextrule_assessment)
                    breach_section_id = breachSectionId(*nextrule_assessment.role_ids)
                    self.apply_action(cur_action, to_breach_section_id = breach_section_id)

                    if verbose:
                        print(f"{srcid} --{actionstr}--> {breach_section_id}")

                    assert i == len(trace) - 1, "Trace prefix results in a breach, but there are more events after."
                    break
                else:
                    assert False, "Can't get here?"

        if final_var_vals:
            for gvarid,expected_val in final_var_vals.items():
                actual_val = self.gvarvals[castid(GlobalVarId,gvarid)]
                assert actual_val == expected_val, f"Expected global variable {gvarid} to have value {expected_val} at end of trace, but had value {actual_val}."

        if len(self.future_obligations) > 0:
            print(f"Trace ended with obligations remaining")
            roles = set()
            for o in self.future_obligations:
                roles.add(o.rule.role_id)
            self.last_or_current_section_id == breachSectionId(*list(roles))
            return

        if finalSectionId:
            assert self.last_or_current_section_id == finalSectionId, f"Trace expected to end in section {finalSectionId} but ended in section {self.last_or_current_section_id}"





    def assess_event_legal_wrt_nextrules(self, event:Event) -> EventLegalityAssessment:
        # for far in chain(self.future_obligations, self.future_permissions):
        #     if self.current_event_compatible_with_enabled_NextActionRule(event, far):
        #         print("COMPAT!")

        entrance_enabled_strong_obligs : List[PartyNextActionRule] = list()
        entrance_enabled_weak_obligs : List[PartyNextActionRule] = list()
        entrance_enabled_permissions : List[PartyNextActionRule] = list()
        entrance_enabled_env_action_rules : List[EnvNextActionRule] = list()

        # entrance_enabled_future_strong_obligs : List[PartyNextActionRule] = list()
        # entrance_enabled_future_permissions : List[PartyNextActionRule] = list()

        nar: NextActionRule
        ctx = EvalContext(self.gvarvals,
                          self.last_appliedaction_params if self.last_or_current_section.is_anon() else None,
                          use_current_event_params_for_rulebound_action_params = False)

        for nar in self.last_or_current_section.action_rules():
            entrance_enabled = True
            if nar.entrance_enabled_guard:
                # next_timestamp param to evalTerm is None because enabled-guards
                # are evaluated only at section entrance time.
                if not chcast(bool, self.evalTerm(nar.entrance_enabled_guard, ctx)):
                    entrance_enabled = False

            if entrance_enabled:
                if isinstance(nar, EnvNextActionRule):
                    entrance_enabled_env_action_rules.append(nar)
                else:
                    assert isinstance(nar, PartyNextActionRule)
                    if nar.deontic_keyword == 'may' or nar.deontic_keyword == 'should':
                        entrance_enabled_permissions.append(nar)
                    elif nar.deontic_keyword == 'must':
                        entrance_enabled_strong_obligs.append(nar)
                    elif nar.deontic_keyword == 'weakly-must':
                        entrance_enabled_weak_obligs.append(nar)
                    else:
                        assert False

        # CASE 0a: Exactly one must-later:
        # if len(entrance_enabled_future_strong_obligs) > 0:
        #     assert (len(entrance_enabled_future_permissions) == len(entrance_enabled_weak_obligs) ==
        #             len(entrance_enabled_strong_obligs) == len(entrance_enabled_permissions) ==
        #             len(entrance_enabled_env_action_rules) == 0), "usage of must-later currently very restricted"
        #     fo = entrance_enabled_future_strong_obligs[0]
        #     self.future_obligations.append(fo)
        #     return EventOk(True)
        #
        # if len(entrance_enabled_future_permissions) > 0:
        #     assert (len(entrance_enabled_future_strong_obligs) == len(entrance_enabled_weak_obligs) ==
        #             len(entrance_enabled_strong_obligs) == len(entrance_enabled_permissions) ==
        #             len(entrance_enabled_env_action_rules) == 0), "usage of may-later currently very restricted"
        #     fp = entrance_enabled_future_permissions[0]
        #     self.future_permissions.append(fp)
        #     return EventOk(True)

        # CASE 1: 1 or more entrance-enabled strong obligations
        if len(entrance_enabled_strong_obligs) > 1:
            contract_bug("There are multiple active strong obligations. This is an error.")
            return ContractFlawedError()
        if len(entrance_enabled_strong_obligs) == 1:
            if len(entrance_enabled_weak_obligs) > 0 or len(entrance_enabled_permissions) > 0 or len(entrance_enabled_env_action_rules) > 0:
                contract_bug("There is exactly one active strong obligation, but there is also at least one "
                             "active permission, environment-action, or weak obligation. This is an error.")
                return ContractFlawedError()
            else:
                assert isinstance(entrance_enabled_strong_obligs[0],PartyNextActionRule)
                if not self.current_event_compatible_with_enabled_NextActionRule(entrance_enabled_strong_obligs[0]):
                    contract_bug(f"There is exactly one active strong obligation\n"
                                 f"\t{entrance_enabled_strong_obligs[0]}\n"
                                 f"but it is not compatible with the current event\n"
                                 f"\t{event}\n"
                                 f"Environment looks like:\n"
                                 f"{self.environ_tostr()}")
                else:
                    deadline_ok = self.evalDeadlineClause(entrance_enabled_strong_obligs[0].deadline_clause, ctx)
                    # print("deadline_ok", deadline_ok)
                    if deadline_ok: return EventOk() #(None,None)
                    else: return BreachResult({event.role_id}, f"Strong obligation of {event.role_id} expired or not yet active.")


        # CASE 2: 0 entrance-enabled strong obligations (SO action-rules)
        present_enabled_weak_obligs : List[NextActionRule] = list(filter( lambda c: self.evalDeadlineClause(c.deadline_clause, ctx), entrance_enabled_weak_obligs ))
        present_enabled_permissions : List[NextActionRule] = list(filter( lambda c: self.evalDeadlineClause(c.deadline_clause, ctx), entrance_enabled_permissions ))
        present_enabled_env_action_rules : List[NextActionRule] = list(filter( lambda c: self.evalDeadlineClause(c.deadline_clause, ctx), entrance_enabled_env_action_rules ))

        # present_enabled_nonSO_action_rules : Iterator[NextActionRule] = chain(
        #     present_enabled_permissions.__iter__(),
        #     present_enabled_weak_obligs.__iter__(),
        #     present_enabled_env_action_rules.__iter__())
        present_enabled_nonSO_action_rules: List[NextActionRule] = present_enabled_permissions + present_enabled_weak_obligs + present_enabled_env_action_rules
        # print("present_enabled_nonSO_action_rules", present_enabled_nonSO_action_rules)

        # CASE 2a: `event` compatible with exactly one present-enabled non-SO action_rule
        # for x in present_enabled_nonSO_action_rules:
        #     print("maybe...", self.current_event_compatible_with_enabled_NextActionRule(x) )

        compatible_present_enabled_nonSO_action_rules = list(filter(
            lambda x: self.current_event_compatible_with_enabled_NextActionRule(x),
            present_enabled_nonSO_action_rules ))
        if len(compatible_present_enabled_nonSO_action_rules) == 1:
            return EventOk() #(None,None)

        if len(compatible_present_enabled_nonSO_action_rules) == 0 and len(present_enabled_weak_obligs) == 0:
            return BreachResult([event.role_id],
                                f"{event.role_id} tried to do an action {event.action_id} that no rule in the current state permits them to do.")
            # assert len(present_enabled_permissions) > 0 or len(present_enabled_env_action_rules) > 0
            # self.evalError("TODO: correctly assign breach in this case! And check that there's not a problem with role ids")

        # print("compatible_present_enabled_nonSO_action_rules", compatible_present_enabled_nonSO_action_rules)

        # CASE 2b: `event` compatible with more than one present-enabled non-SO action-rules
        # ...This is actually ok as long as deadlinesPartitionFuture isn't used.
        if len(compatible_present_enabled_nonSO_action_rules) > 1:
            return EventOk() #(None,None)

        # CASE 2c: `event` compatible with 0 present-enabled non-SO action-rules
        # This is a breach. We need to determine what subset of the roles is at fault
        # You're at fault if you had an entrance-enabled weak obligation (which now expired,
        # from previous cases).
        breach_roles = filter(
            lambda r: len( list(filter(lambda c: c.role_id == r, entrance_enabled_weak_obligs)) ) > 0,
            self.top.roles )

        # print("event:", event)
        # print("vars:", self.gvarvals)
        # print("entrance_enabled_permissions", mapjoin(str,entrance_enabled_permissions,', '))
        # print("present_enabled_permissions", list(present_enabled_permissions))
        # print("present_enabled_weak_obligs", list(present_enabled_weak_obligs))
        # print("present_enabled_env_action_rules", list(present_enabled_env_action_rules))
        # print()

        return BreachResult(list(breach_roles), f"{list(breach_roles)} had weak obligations that they didn't fulfill in time.")

    # Only if the current section is an anonymous section (i.e. given by a FollowingSection declaration) is it possible
    # for the where_clause of action_rule to contain action-bound action parameters.
    def current_event_compatible_with_enabled_NextActionRule(self, action_rule:NextActionRule) -> bool:
        assert self.cur_event is not None
        role_action_match = self.cur_event.role_id == action_rule.role_id and self.cur_event.action_id == action_rule.action_id
        if not role_action_match: return False
        if not action_rule.where_clause: return True



        # ctx2 = EvalContext(self.gvarvals, self.cur_event.params, ctx.abapvals)
        return chcast(bool,self.evalTerm(action_rule.where_clause, None))


    def current_event_compatible_with_floatingrule(self, pif_action_rule: PartlyInstantiatedPartyFutureActionRule) -> bool:
        assert self.cur_event is not None
        role_action_match = self.cur_event.role_id == pif_action_rule.rule.role_id and self.cur_event.action_id == pif_action_rule.rule.action_id
        if not role_action_match: return False
        if not pif_action_rule.pe_where_clause: return True

        # will sub in self.cur_event.params for *rule*-bound action params in pif_action_rule.pe_where_clause
        # all other variables value in pif_action_rule.pe_where_clause are supplied by pif_action_rule.ctx
        # note that whether the current event is compatible with a future is not allowed to depend on
        # anything from the current execution context except self.cur_event.params. that is the reason for the None
        # argument.

        # TODO UNCERTAIN

        ctx = EvalContext(self.gvarvals, None, use_current_event_params_for_rulebound_action_params=True)
        return chcast(bool,self.evalTerm(pif_action_rule.pe_where_clause, ctx))

    def environ_tostr(self) -> str:
        return f"{str(self.gvarvals)}\n{str(self.last_appliedaction_params)}\n{str(self.contract_param_vals)}"

    # def action_available_from_cur_section(self, actionid:ActionId, next_timestamp:TimeInt) -> bool:
    #     todo_once("this needs to depend on action parameters")
    #     for c in self.last_or_current_section().action_rules():
    #         todo_once("check enabled guard")
    #         if isinstance(c, PartyNextActionRule) or isinstance(c, EnvNextActionRule):
    #             if c.action_id != actionid:
    #                 continue
    #         else:
    #             raise NotImplementedError
    #
    #         deadline_ok = self.evalDeadlineClause(c.deadline_clause, next_timestamp)
    #         # print("deadline_ok", deadline_ok)
    #         if deadline_ok:
    #             return True
    #     return False

    def apply_action(self, action:Action, to_breach_section_id:Optional[SectionId] = None) -> ApplyActionResult:
        assert self.cur_event is not None

        if to_breach_section_id is not None:
            self.last_or_current_section_id = to_breach_section_id
            self.last_section_entrance_timestamp = self.toTimeDelta(self.cur_event.timestamp)
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
                future = PartlyInstantiatedPartyFutureActionRule(far, new_where_clause)
                # far = copy.copy(far)
                # far.where_clause = new_where_clause
            else:
                future = PartlyInstantiatedPartyFutureActionRule(far, None)

            if far.deontic_keyword == 'must-later':
                self.future_obligations.append(future)
            else:
                assert  far.deontic_keyword == 'may-later'
                self.future_permissions.append(future)
            floatingrules_added.append(future)
            # print("new future:", ef)

        if action.dest_section_id != LOOP_KEYWORD:
            self.last_or_current_section_id = action.dest_section_id
        self.last_section_entrance_timestamp = self.toTimeDelta(self.cur_event.timestamp)

        return ApplyActionResult(floatingrules_added if len(floatingrules_added) > 0 else None)


    def evalGlobalVarDecs(self, decs : Dict[GlobalVarId, GlobalVarDec]):
        for (var,dec) in cast(Dict[GlobalVarId, GlobalVarDec],decs).items():
            print("dec: ", dec, dec.initval)
            if dec.initval is None:
                self.gvarvals[var] = None
                print(f"Global var {var} has no initial value.")
                dictSetOrInc(self.gvar_write_cnt, var, init=0)
            else:
                self.gvarvals[var] = self.evalTerm(dec.initval, None)
                print(f"Global var {var} has initial value {self.gvarvals[var]}.")
                dictSetOrInc(self.gvar_write_cnt, var, init=1)
            # print(var, type(self.gvarvals[var]))
            # print('evalGlobalVarDecs: ', var, dec, self.gvar_write_cnt[var])

    def evalContractParamDecs(self, decs : Dict[ContractParamId, ContractParamDec]):
        for (name,dec) in decs.items():
            print("contract param dec", dec, type(dec.value_expr))
            if not(dec.value_expr is None):
                self.contract_param_vals[name] = self.evalTerm(dec.value_expr, None)
            else:
                self.contract_param_vals[name] = None
            # print(name, type(self.contract_param_vals[name]))

    def evalCodeBlock(self, transform:GlobalStateTransform):
        # print("\nevalCodeBlock\n")
        # action = self.top.action(nextTSEvent.action_id)
        # if not action.global_state_transform:
        #     return
        for statement in transform.statements:
            self.evalStatement(statement)

    def evalStatement(self, stmt:GlobalStateTransformStatement):
        # An Action's GlobalStateTransform block is *always* evaluated in the most recent global variable and
        # action parameter substitution context that's visible to it, as given by self.gvarvals and self.cur_event,
        # even when applying an action from a PartlyInstantiatedPartyFutureActionRule.
        # Therefore, this function does not take an EvalContext argument.

        # gvardec : GlobalVarDec

        if isinstance(stmt, (VarAssignStatement, IncrementStatement,DecrementStatement,TimesEqualsStatement)):
            gvardec = self.top.gvarDecObj(stmt.varname)
            assert gvardec is not None, f"Global variable declaration for {stmt.varname} not found."

            if gvardec.isWriteOnceMore() and self.gvar_write_cnt[stmt.varname] >= 2:
                raise Exception(f"Attempt to write twice more (after init) to writeOnceMore variable `{stmt.varname}`")
            dictSetOrInc(self.gvar_write_cnt, stmt.varname, init=1)
            todo_once("use vardec for runtime type checking")
            rhs_value = self.evalTerm(stmt.value_expr, None)

        if isinstance(stmt, VarAssignStatement):
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
            test = chcast(bool, self.evalTerm(stmt.test, None))
            self.evalCodeBlock(GlobalStateTransform(stmt.true_branch if test else stmt.false_branch))
        else:
            raise NotImplementedError("Unhandled GlobalStateTransformStatement: " + str(stmt))


        # if isinstance(stmt, LocalVarDec):
        #     assert self.top.gvarDecObj(stmt.varname) is None
        #     # print('evalStatement LocalVarDec: ' + str(stmt))
        #     rhs_value = self.evalTerm(stmt.value_expr, self.last_section_entrance_timestamp)
        #     self.gvarvals[stmt.varname] = rhs_value

    def evalTerm(self,
                 term:Term,
                 ctx:Optional[EvalContext]) -> Any:
        assert term is not None
        # assert self.cur_event is not None  # nope! it can be None when evaluating terms in contract param dec
        # and global var decs

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
                    print(ctx)
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
                assert self.cur_event and self.cur_event.params is not None
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

            else:
                contract_bug(f"evalTerm unhandled case for: {str(term)} of type {type(term)}")
                return term
        except Exception as e:
            raise e
            contract_bug("Exception while evaluating " + str(term))

    def evalLit(self, litterm:Literal) -> Data:
        if isinstance(litterm, DeadlineLit):
            todo_once("DeadlineLit not correctly handled yet")
            # print("DeadlineLit: ", term, next_event_timestamp == self.last_section_entrance_timestamp)
            # return self.cur_event.timedelta == self.last_section_entrance_timestamp
            return True
        elif isinstance(litterm, SimpleTimeDeltaLit):
            return litterm.timedelta
        else:
            return litterm.lit

    def evalDeadlineClause(self, deadline_clause:Term, ctx:EvalContext) -> bool:
        assert deadline_clause is not None
        # return True
        rv = chcast(bool, self.evalTerm(deadline_clause, ctx))
        # print('evalDeadlineClause: ', rv)
        return rv

    def evalFnApp(self,
                  fnapp:FnApp,
                  ctx:Optional[EvalContext]) -> Any:
        fn = fnapp.head
        # print("the fnapp: ", str(fnapp), " with args ", fnapp.args)
        evaluated_args = tuple(self.evalTerm(x,ctx) for x in fnapp.args)
        if fn in FN_SYMB_INTERP:
            if fn == '+':
                print("args", fnapp.args)
                print("eval'ed argss", evaluated_args )
            return cast(Any,FN_SYMB_INTERP[fn])(*evaluated_args)
        elif fn in DEADLINE_OPERATORS or fn in DEADLINE_PREDICATES:
            assert self.cur_event is not None
            if fn in DEADLINE_OP_INTERP:
                return cast(Any, DEADLINE_OP_INTERP[fn])(
                    self.last_section_entrance_timestamp,
                    self.toTimeDelta(self.cur_event.timestamp),
                    evaluated_args)
            else:
                contract_bug(f"Unhandled deadline fn symbol: {fn}")
        elif fn == "contractStartTimestamp":
            return self.toTimeDelta(0)
        elif fn == "unitsAfterEntrance":
            assert len(evaluated_args) == 1
            return evaluated_args[0] + self.last_section_entrance_timestamp
        elif fn == 'entranceTimeNoLaterThan-ts?':
            assert len(evaluated_args) == 1
            return self.last_section_entrance_timestamp <= evaluated_args[0]
        elif fn == 'entranceTimeAfter-ts?':
            assert len(evaluated_args) == 1
            return self.last_section_entrance_timestamp > evaluated_args[0]
        else:
            contract_bug(f"Unhandled fn symbol: {fn}")
        return 0

    def mk_event(self, action_id: str, role_id: str = ENV_ROLE,
              timestamp: int = 0, params: Optional[Dict[str, Data]] = None,
              eventType: Optional[EventType] = None) -> Event:
        if eventType is None:
            eventType = EventType.env_next if role_id == ENV_ROLE else EventType.party_next
        params = params or dict()
        return Event(action_id=castid(ActionId, action_id), role_id=castid(RoleId, role_id),
                     timestamp=timestamp,
                     params_by_abap_name=cast(ABAPNamedSubst, params) if params else None,
                     params=list(params.values()) if params else None,
                     type=eventType)

    def mk_foevent(self, action_id: str, role_id: str = ENV_ROLE,
                timestamp: int = 0, params: Optional[Dict[str, Data]] = None) -> Event:
        return self.mk_event(action_id, role_id, timestamp, params, EventType.fulfill_floating_obligation)

    def mk_fpevent(self, action_id: str, role_id: str = ENV_ROLE,
                timestamp: int = 0, params: Optional[Dict[str, Data]] = None) -> Event:
        return self.mk_event(action_id, role_id, timestamp, params, EventType.use_floating_permission)

    def evalError(self, msg: str, thing:Any = None):
        raise EvalError(msg, thing)

    def assertOrEvalError(self, test:bool, msg:str, thing:Any = None):
        if not test:
            self.evalError(msg,thing) if thing else self.evalError(msg)



def evalTrace(it:Union[Trace,CompleteTrace], prog:L4Contract):
    env = ExecEnv(prog)
    if isinstance(it, CompleteTrace):
        for contract_param in it.contract_param_subst:
            print("is this happening?")
            supplied_val = it.contract_param_subst[contract_param]
            paramdec = prog.contract_params[castid(ContractParamId, contract_param)]
            if isinstance(paramdec.value_expr,Literal):
                paramdec.value_expr.lit = supplied_val
            else:
                paramdec.value_expr = L4ContractConstructor.mk_literal(supplied_val)

        return env.evalTrace(trace = it.events,
                             finalSectionId = cast(SectionId, it.final_section),
                             final_var_vals = cast(Optional[GVarSubst], it.final_values),
                             verbose=True)
    else:
        return env.evalTrace(it, verbose=True)

