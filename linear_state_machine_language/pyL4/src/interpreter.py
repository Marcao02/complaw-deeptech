import logging
from itertools import chain

import copy

from src.model.Action import Action
from src.model.BoundVar import BoundVar, LocalVar, GlobalVar, ContractParam, ActionDeclActionParam, \
    ActionRuleDeclActionParam
from src.model.ContractParamDec import ContractParamDec
from src.model.EventsAndTraces import Event, Trace, CompleteTrace, breachSectionId
from src.model.GlobalStateTransform import GlobalStateTransform
from src.model.GlobalStateTransformStatement import *
from src.model.GlobalVarDec import GlobalVarDec
from src.model.Literal import StringLit, Literal, DeadlineLit
from src.model.PartialEvalTerm import PartialEvalTerm
from src.model.Term import FnApp

logging.basicConfig(
    format="[%(levelname)s] %(funcName)s: %(message)s",
    level=logging.INFO )
from typing import Tuple, Any, cast, Dict, Iterable, Iterator, Sequence, NewType

from src.model.L4Contract import L4Contract

from src.model.ActionRule import PartyNextActionRule, EnvNextActionRule, NextActionRule, PartyFutureActionRule, ActionRule
from src.model.Section import Section
from src.sexpr_to_L4Contract import L4ContractConstructor
from src.parse_sexpr import prettySExprStr, parse_file
from src.model.constants_and_defined_types import GlobalVarId, ActionId, DEADLINE_OPERATORS, DEADLINE_PREDICATES, RoleId, \
    ActionParamId_BoundBy_ActionRule, ContractParamId, SectionId, ActionParamId_BoundBy_ActionDecl, FULFILLED_SECTION_LABEL, ActionParamSubst, \
    TimeStamp, ActionParamValue, ENV_ROLE
from src.model.util import hasNotNone, dictSetOrInc, todo_once, chcast, contract_bug, mapjoin

feedback = logging

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
    def __init__(self, future_added : Optional[PartyFutureActionRule], future_removed : Optional[PartyFutureActionRule]) -> None:
        self.future_added = future_added
        self.future_removed = future_removed


FN_SYMB_INTERP = {
    '+': lambda *args: sum(args),
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
    'or': lambda x,y: x or y,
    'unitsAfter' : lambda x,y: x + y,
    'round': round
}

DEADLINE_OP_INTERP = {
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
    if event.params:
        params_str = ", ".join([f"{key}: {event.params[key]}" for key in event.params.keys()])
        return f"{event.action_id}({params_str})"
    else:
        return f"{event.action_id}"



class ExecEnv:
    def __init__(self, prog:L4Contract) -> None:
        self._top : L4Contract = prog
        self._varvals : Dict[LocalOrGlobalVarId, Any] = dict()
        self._var_write_cnt : Dict[LocalOrGlobalVarId, int] = dict()
        self._contract_param_vals: Dict[ContractParamId, Any] = dict()
        self._section_id: SectionId = prog.start_section_id
        self._sec_entrance_timestamp = cast(TimeStamp,0)
        self._cur_action_param_subst_from_event : Optional[Dict[ActionParamId_BoundBy_ActionDecl, ActionParamValue]]

        self._future_permissions : List[PartyFutureActionRule] = []
        self._future_obligations : List[PartyFutureActionRule] = []

    def futures(self) -> Iterator[PartyFutureActionRule]:
        return chain(self._future_obligations, self._future_permissions)

    def delete_future(self, future:PartyFutureActionRule) -> bool:
        assert future not in self._future_permissions or future not in self._future_obligations
        if future in self._future_obligations:
            self._future_obligations.remove(future)
            return True
        elif future in self._future_permissions:
            self._future_permissions.remove(future)
            return True

        return False

    def cur_section(self) -> Section:
        return self._top.section(self._section_id)

    def evalLSM(self, trace:Trace, finalSectionId:Optional[SectionId] = None, final_var_vals: Optional[Dict[GlobalVarId,Any]] = None, verbose=False):
        prog = self._top
        self.evalContractParamDecs(prog.contract_params)
        self.evalGlobalVarDecs(prog.global_var_decs)

        max_section_id_len = max(max(
            len(self._top.action(event.action_id).dest_section_id) for event in trace
        ), len(finalSectionId) if finalSectionId else 0)
        max_action_str_len = max(
            len(event_to_action_str(event)) for event in trace
        )

        for i in range(len(trace)):
            eventi = trace[i]
            self._cur_action_param_subst_from_event = eventi.params
            next_action_id, next_timestamp = eventi.action_id, eventi.timestamp

            if not(self._sec_entrance_timestamp is None or self._sec_entrance_timestamp <= next_timestamp):
                feedback.error(
                    f"Event timestamps must be non-decreasing, but current {self._sec_entrance_timestamp} > next {next_timestamp}")
                continue

            action = self._top.action(next_action_id)
            if not action:
                feedback.error(f"Don't recongize action id {next_action_id}")
                continue

            found : Optional[PartyFutureActionRule] = None
            for far in self.futures():
                # print("evaluating", str(far), "in event context", str(eventi))
                if self.event_action_rule_compatible(eventi, far):
                    # print("evaluated to True")
                    # print("the global var vals:",self._varvals)
                    assert found is None, "should be at most one..."
                    found = far
            if found:
                # print("deletion successful?", self.delete_future(found))
                self.delete_future(found)
                self._cur_action_param_subst_from_event = None
                self.apply_action(action, next_timestamp)
                # print(f"after did future action {far.action_id}, section is {self._section_id}")
                # continue
            elif not self._top.action_is_sometimes_available_from_section(self._section_id, next_action_id):
                feedback.error(f"Cannot *ever* do action {next_action_id} from section {self._section_id} (no {next_action_id}-action_rule exists).")
                continue

            if not found:
                result = self.assess_event_legality(eventi)
            actionstr = event_to_action_str(eventi)
            if found or isinstance(result, EventOk):
                srcid = self._section_id
                self.apply_action(action, next_timestamp)
                self._cur_action_param_subst_from_event = None
                if verbose:
                    sec_pad = ' '*(max_section_id_len-len(self._section_id))
                    # act_pad = ' '*(max_action_str_len-len(actionstr))
                    act_pad = ''
                    future_str = f' and deleted [{found}]' if found else ''
                    print(f"{srcid}{sec_pad} --{actionstr}-->{act_pad} {action.dest_section_id} {future_str}")
                continue

            elif isinstance(result, BreachResult):
                print("Breach result:", result)
                breach_section_id = breachSectionId(*result.role_ids)
                if verbose:
                    sec_pad = ' '*(max_section_id_len-len(self._section_id))
                    act_pad = ' '*(max_action_str_len-len(actionstr))
                    print(f"{self._section_id}{sec_pad} --{actionstr}-->{act_pad} {breach_section_id}")
                self._section_id = breach_section_id

                todo_once("timestamp in transition to breach")
                self._cur_action_param_subst_from_event = None

                assert i == len(trace) - 1, "Trace prefix results in a breach, but there are more events after."
                break
            else:
                assert False, "Can't get here?"

            # else:
            #     feedback.error(f"{next_action_id}-action_rule exists in {self._section_id} but is disabled:\n", result)

            # feedback.error("Stopping evaluation")
            # return

        if finalSectionId:
            assert self._section_id == finalSectionId, f"Trace expected to end in section {finalSectionId} but ended in section {self._section_id}"

        if final_var_vals:
            for gvarid,expected_val in final_var_vals.items():
                actual_val = self._varvals[castid(LocalOrGlobalVarId,gvarid)]
                assert actual_val == expected_val, f"Expected global variable {gvarid} to have value {expected_val} at end of trace, but had value {actual_val}."

        assert len(self._future_obligations) == 0, f"Trace ended with obligations remaining"

    def assess_event_legality(self, event:Event) -> EventLegalityAssessment:
        # for far in chain(self._future_obligations, self._future_permissions):
        #     if self.event_action_rule_compatible(event, far):
        #         print("COMPAT!")

        entrance_enabled_strong_obligs : List[PartyNextActionRule] = list()
        entrance_enabled_weak_obligs : List[PartyNextActionRule] = list()
        entrance_enabled_permissions : List[PartyNextActionRule] = list()
        entrance_enabled_env_action_rules : List[EnvNextActionRule] = list()

        # entrance_enabled_future_strong_obligs : List[PartyNextActionRule] = list()
        # entrance_enabled_future_permissions : List[PartyNextActionRule] = list()

        c: NextActionRule

        for c in self.cur_section().action_rules():
            entrance_enabled = True
            if c.entrance_enabled_guard:
                # next_timestamp param to evalTerm is None because enabled-guards
                # are evaluated only at section entrance time.
                if not chcast(bool, self.evalTerm(c.entrance_enabled_guard, None)):
                    entrance_enabled = False

            if entrance_enabled:
                if isinstance(c, EnvNextActionRule):
                    entrance_enabled_env_action_rules.append(c)
                else:
                    assert isinstance(c, PartyNextActionRule)
                    if c.deontic_keyword == 'may' or c.deontic_keyword == 'should':
                        entrance_enabled_permissions.append(c)
                    elif c.deontic_keyword == 'must':
                        entrance_enabled_strong_obligs.append(c)
                    elif c.deontic_keyword == 'weakly-must':
                        entrance_enabled_weak_obligs.append(c)
                    # elif c.deontic_keyword == 'may-later':
                    #     entrance_enabled_future_permissions.append(c)
                    # elif c.deontic_keyword == 'must-later':
                    #     entrance_enabled_future_strong_obligs.append(c)
                    else:
                        assert False

        # CASE 0a: Exactly one must-later:
        # if len(entrance_enabled_future_strong_obligs) > 0:
        #     assert (len(entrance_enabled_future_permissions) == len(entrance_enabled_weak_obligs) ==
        #             len(entrance_enabled_strong_obligs) == len(entrance_enabled_permissions) ==
        #             len(entrance_enabled_env_action_rules) == 0), "usage of must-later currently very restricted"
        #     fo = entrance_enabled_future_strong_obligs[0]
        #     self._future_obligations.append(fo)
        #     return EventOk(True)
        #
        # if len(entrance_enabled_future_permissions) > 0:
        #     assert (len(entrance_enabled_future_strong_obligs) == len(entrance_enabled_weak_obligs) ==
        #             len(entrance_enabled_strong_obligs) == len(entrance_enabled_permissions) ==
        #             len(entrance_enabled_env_action_rules) == 0), "usage of may-later currently very restricted"
        #     fp = entrance_enabled_future_permissions[0]
        #     self._future_permissions.append(fp)
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
                if not self.event_action_rule_compatible(event, entrance_enabled_strong_obligs[0]):
                    contract_bug(f"There is exactly one active strong obligation\n"
                                 f"\t{entrance_enabled_strong_obligs[0]}\n"
                                 f"but it is not compatible with the current event\n"
                                 f"\t{event}\n"
                                 f"Environment looks like:\n"
                                 f"{self.environ_tostr()}")
                else:
                    deadline_ok = self.evalDeadlineClause(c.deadline_clause, event.timestamp)
                    # print("deadline_ok", deadline_ok)
                    if deadline_ok:
                        return EventOk(None,None)
                    else:
                        return BreachResult(set([event.role_id]), f"Strong obligation of {event.role_id} expired.")


        # CASE 2: 0 entrance-enabled strong obligations (SO action-rules)
        present_enabled_weak_obligs : List[NextActionRule] = list(filter( lambda c: self.evalDeadlineClause(c.deadline_clause, event.timestamp), entrance_enabled_weak_obligs ))
        present_enabled_permissions : List[NextActionRule] = list(filter( lambda c: self.evalDeadlineClause(c.deadline_clause, event.timestamp), entrance_enabled_permissions ))
        present_enabled_env_action_rules : List[NextActionRule] = list(filter( lambda c: self.evalDeadlineClause(c.deadline_clause, event.timestamp), entrance_enabled_env_action_rules ))

        # present_enabled_nonSO_action_rules : Iterator[NextActionRule] = chain(
        #     present_enabled_permissions.__iter__(),
        #     present_enabled_weak_obligs.__iter__(),
        #     present_enabled_env_action_rules.__iter__())
        present_enabled_nonSO_action_rules: List[NextActionRule] = present_enabled_permissions + present_enabled_weak_obligs + present_enabled_env_action_rules

        # CASE 2a: `event` compatible with exactly one present-enabled non-SO action_rule
        compatible_present_enabled_nonSO_action_rules = list(filter(
            lambda c: self.event_action_rule_compatible(event,c),
            present_enabled_nonSO_action_rules ))
        if len(compatible_present_enabled_nonSO_action_rules) == 1:
            return EventOk(None,None)

        # print("compatible_present_enabled_nonSO_action_rules", compatible_present_enabled_nonSO_action_rules)

        # CASE 2b: `event` compatible with more than one present-enabled non-SO action-rules
        # ...This is actually ok as long as deadlinesPartitionFuture isn't used.
        if len(compatible_present_enabled_nonSO_action_rules) > 1:
            return EventOk(None,None)

        # CASE 2c: `event` compatible with 0 present-enabled non-SO action-rules
        # This is a breach. We need to determine what subset of the roles is at fault
        # You're at fault if you had an entrance-enabled weak obligation (which now expired,
        # from previous cases).
        breach_roles = filter(
            lambda r: len( list(filter(lambda c: c.role_id == r, entrance_enabled_weak_obligs)) ) > 0,
            self._top.roles )

        # print("event:", event)
        # print("vars:", self._varvals)
        # print("entrance_enabled_permissions", mapjoin(str,entrance_enabled_permissions,', '))
        # print("present_enabled_permissions", list(present_enabled_permissions))
        # print("present_enabled_weak_obligs", list(present_enabled_weak_obligs))
        # print("present_enabled_env_action_rules", list(present_enabled_env_action_rules))
        # print()

        return BreachResult(list(breach_roles), f"{list(breach_roles)} had weak obligations that they didn't fulfill in time.")

    def event_action_rule_compatible(self, event:Event, action_rule:ActionRule):
        if not action_rule.where_clause:
            return event.role_id == action_rule.role_id and event.action_id == action_rule.action_id

        # print("Action-param subst:", self._cur_action_param_subst_from_event)
        assert self._cur_action_param_subst_from_event == event.params
        rv = (event.role_id == action_rule.role_id and event.action_id == action_rule.action_id and
              self.evalTerm(action_rule.where_clause, event.timestamp) )

        return rv

    def environ_tostr(self) -> str:
        return f"{str(self._varvals)}\n{str(self._cur_action_param_subst_from_event)}\n{str(self._contract_param_vals)}"

    # def action_available_from_cur_section(self, actionid:ActionId, next_timestamp:TimeStamp) -> bool:
    #     todo_once("this needs to depend on action parameters")
    #     for c in self.cur_section().action_rules():
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

    def apply_action(self, action:Action, next_timestamp: TimeStamp):
        # self._cur_action_param_subst_from_event should be set before calling apply_action
        if action.global_state_transform:
            self.evalCodeBlock(action.global_state_transform)

        for ef in filter(
            lambda far: not far.entrance_enabled_guard or chcast(bool,self.evalTerm(far.entrance_enabled_guard, None)),
            action.futures
        ):
            if ef.where_clause:
                todo_once("should keep local and global var vals separate")
                new_where_clause = PartialEvalTerm(ef.where_clause, cast(Dict[GlobalVarId,Any],self._varvals.copy()))
                ef = copy.copy(ef)
                ef.where_clause = new_where_clause

            if ef.deontic_keyword == 'must-later':
                self._future_obligations.append(ef)
            else:
                assert  ef.deontic_keyword == 'may-later'
                self._future_permissions.append(ef)
            # print("new future:", ef)

        self._section_id = action.dest_section_id
        self._sec_entrance_timestamp = next_timestamp


    def evalGlobalVarDecs(self, decs : Dict[GlobalVarId, GlobalVarDec]):
        for (var,dec) in cast(Dict[LocalOrGlobalVarId, GlobalVarDec],decs).items():
            if dec.initval is None:
                self._varvals[var] = None
                # print(f"Global var {var} has no initial value.")
                dictSetOrInc(self._var_write_cnt, var, init=0)
            else:
                self._varvals[var] = self.evalTerm(dec.initval, cast(TimeStamp,0))
                # print(f"Global var {var} has initial value {self._varvals[var]}.")
                dictSetOrInc(self._var_write_cnt, var, init=1)
            # print('evalGlobalVarDecs: ', var, dec, self._var_write_cnt[var])

    def evalContractParamDecs(self, decs : Dict[ContractParamId, ContractParamDec]):
        for (name,dec) in decs.items():
            # print(dec)
            if not(dec.value_expr is None):
                self._contract_param_vals[name] = self.evalTerm(dec.value_expr, cast(TimeStamp,0))
            else:
                self._contract_param_vals[name] = None

    def evalCodeBlock(self, transform:GlobalStateTransform):
        # print("\nevalCodeBlock\n")
        # action = self._top.action(nextTSEvent.action_id)
        # if not action.global_state_transform:
        #     return
        for statement in transform.statements:
            self.evalStatement(statement)

    def evalStatement(self, stmt:GlobalStateTransformStatement):

        if isinstance(stmt, LocalVarDec):
            assert self._top.varDecObj(stmt.varname) is None
            # print('evalStatement LocalVarDec: ' + str(stmt))
            value = self.evalTerm(stmt.value_expr, self._sec_entrance_timestamp)
            self._varvals[stmt.varname] = value

        elif isinstance(stmt, VarAssignStatement):
            vardec = self._top.varDecObj(stmt.varname)
            # print(self._var_write_cnt[stmt.varname])
            if isinstance(vardec, GlobalVarDec):
                if vardec.isWriteOnceMore() and self._var_write_cnt[stmt.varname] >= 2:
                    raise Exception(f"Attempt to write twice more (after init) to writeOnceMore variable `{stmt.varname}`")
            else:
                raise NotImplementedError("Non-GlobalVar assign statement unhandled")
            dictSetOrInc(self._var_write_cnt, stmt.varname, init=1)
            # todo: use vardec for runtime type checking
            value = self.evalTerm(stmt.value_expr, self._sec_entrance_timestamp)
            self._varvals[stmt.varname] = value
            print(f"Assigning {value} to {stmt.varname}")

        elif isinstance(stmt, (IncrementStatement,DecrementStatement,TimesEqualsStatement)):
            vardec = self._top.varDecObj(stmt.varname)
            # todo: use vardec for runtime type checking
            value = self.evalTerm(stmt.value_expr, self._sec_entrance_timestamp)
            if not stmt.varname in self._varvals:
                contract_bug(f"Variable {stmt.varname} should be in the execution environment, but isn't.")
            assert self._varvals[stmt.varname] is not None, "Tried to increment/decrement/scale uninitialized variable `" + stmt.varname + "`"

            if isinstance(stmt, IncrementStatement):
                self._varvals[stmt.varname] = self._varvals[stmt.varname] + int(value)
            elif isinstance(stmt, DecrementStatement):
                self._varvals[stmt.varname] = self._varvals[stmt.varname] - int(value)
            elif isinstance(stmt, TimesEqualsStatement):
                self._varvals[stmt.varname] = self._varvals[stmt.varname] * int(value)
            else:
                raise Exception('should be impossible')

        elif isinstance(stmt, InCodeConjectureStatement):
            # print("conjecture " + str(stmt))
            assert self.evalTerm(stmt.value_expr, None), f"""Conjecture {stmt.value_expr} is false! Variable values and action params:
            {str(self._varvals)}
            {str(self._cur_action_param_subst_from_event)}
            """

        elif isinstance(stmt, IfElse):
            test = chcast(bool, self.evalTerm(stmt.test, None))
            self.evalCodeBlock(GlobalStateTransform(stmt.true_branch if test else stmt.false_branch))
        else:
            raise NotImplementedError("Unhandled GlobalStateTransformStatement: " + str(stmt))

    def evalTerm(self,
                 term:Term,
                 next_event_timestamp:Optional[TimeStamp],
                 partialeval_globals_subst:Optional[Dict[GlobalVarId,Any]] = None) -> Any:
        assert term is not None
        # if partialeval_globals_subst:
        #     print("partialeval_globals_subst is not None. `term` is ", term)
        # next_event_timestamp will be None when evaluating an entrance-guard
        # print('evalTerm: ', term)
        if isinstance(term, FnApp):
            return self.evalFnApp(term, next_event_timestamp, partialeval_globals_subst)
        elif isinstance(term, DeadlineLit):
            todo_once("DeadlineLit correctly handled?")
            # print("DeadlineLit: ", term, next_event_timestamp == self._sec_entrance_timestamp)
            return next_event_timestamp == self._sec_entrance_timestamp
        elif isinstance(term, Literal):
            return term.lit
        elif isinstance(term, LocalVar):
            assert hasNotNone(self._varvals, cast(LocalOrGlobalVarId,term.name)), term.name
            return self._varvals[cast(LocalOrGlobalVarId,term.name)]
        elif isinstance(term, GlobalVar):
            # print(self._contract_param_vals)
            # print(self._varvals)
            if partialeval_globals_subst and term.name in partialeval_globals_subst:
                # print("using val from partial val subst")
                return partialeval_globals_subst[term.name]
            else:
                assert hasNotNone(self._varvals, cast(LocalOrGlobalVarId, term.name)), "Global var " + term.name + " should have a value but doesn't."
                return self._varvals[cast(LocalOrGlobalVarId, term.name)]
        elif isinstance(term, ContractParam):
            # print("ContractParam case of evalTerm: ", term)
            # print(self._contract_param_vals[term.name], type(self._contract_param_vals[term.name]))
            assert hasNotNone(self._contract_param_vals, term.name), term.name
            return self._contract_param_vals[term.name]
        elif isinstance(term,ActionDeclActionParam):
            if self._cur_action_param_subst_from_event:
                if term.name in self._cur_action_param_subst_from_event:
                    return self._cur_action_param_subst_from_event[cast(ActionParamId_BoundBy_ActionDecl, term.name)]
                todo_once("HACK!")
                if "_" + term.name in self._cur_action_param_subst_from_event:
                    return self._cur_action_param_subst_from_event[cast(ActionParamId_BoundBy_ActionDecl, '_' + term.name)]
            contract_bug(f"Trying to get subst value of an ActionDeclActionParam {term.name} but didn't find it in the execution context.")
        elif isinstance(term, ActionRuleDeclActionParam):
            if self._cur_action_param_subst_from_event:
                if term.name in self._cur_action_param_subst_from_event:
                    return self._cur_action_param_subst_from_event[cast(ActionParamId_BoundBy_ActionDecl, term.name)]
                todo_once("HACK!")
                if "_" + term.name in self._cur_action_param_subst_from_event:
                    return self._cur_action_param_subst_from_event[cast(ActionParamId_BoundBy_ActionDecl, '_' + term.name)]
                print(self._cur_action_param_subst_from_event)
            contract_bug(f"Trying to get subst value of ActionRuleDeclActionParam {term.name} but didn't find it in the execution context.")
        elif isinstance(term, PartialEvalTerm):
            assert not partialeval_globals_subst, "haven't handled partial eval of partial eval yet"
            return self.evalTerm(term.term, next_event_timestamp, term.subst)
        else:
            contract_bug(f"evalTerm unhandled case for: {str(term)} of type {type(term)}")
            return term

    def evalDeadlineClause(self, deadline_clause:Term, next_timestamp:TimeStamp) -> bool:
        assert deadline_clause is not None
        # return True
        rv = chcast(bool, self.evalTerm(deadline_clause, next_timestamp))
        # print('evalDeadlineClause: ', rv)
        return rv

    def evalFnApp(self, fnapp:FnApp, next_event_timestamp: Optional[TimeStamp], partialeval_globals_subst:Optional[Dict[GlobalVarId,Any]] = None) -> Any:
        fn = fnapp.head
        # print("the fnapp: ", str(fnapp), " with args ", fnapp.args)
        evaluated_args = tuple(self.evalTerm(x, next_event_timestamp, partialeval_globals_subst) for x in fnapp.args)
        if fn in FN_SYMB_INTERP:
            return cast(Any,FN_SYMB_INTERP[fn])(*evaluated_args)
        elif fn in DEADLINE_OPERATORS or fn in DEADLINE_PREDICATES:
            assert next_event_timestamp is not None
            if fn in DEADLINE_OP_INTERP:
                return cast(Any, DEADLINE_OP_INTERP[fn])(self._sec_entrance_timestamp, next_event_timestamp, evaluated_args)
            else:
                contract_bug(f"Unhandled deadline fn symbol: {fn}")
        elif fn == "unitsAfterEntrance":
            assert len(evaluated_args) == 1
            return evaluated_args[0] + self._sec_entrance_timestamp
        elif fn == 'entranceTimeNoLaterThan-ts?':
            assert len(evaluated_args) == 1
            return self._sec_entrance_timestamp <= evaluated_args[0]
        elif fn == 'entranceTimeAfter-ts?':
            assert len(evaluated_args) == 1
            return self._sec_entrance_timestamp > evaluated_args[0]
        else:
            contract_bug(f"Unhandled fn symbol: {fn}")
        return 0


def evalTrace(it:Union[Trace,CompleteTrace], prog:L4Contract):
    env = ExecEnv(prog)
    if isinstance(it, CompleteTrace):
        for contract_param in it.contract_param_subst:
            supplied_val = it.contract_param_subst[contract_param]
            paramdec = prog.contract_params[castid(ContractParamId, contract_param)]
            if isinstance(paramdec.value_expr,Literal):
                paramdec.value_expr.lit = supplied_val
            else:
                paramdec.value_expr = L4ContractConstructor.mk_literal(supplied_val)

        return env.evalLSM(it.events, finalSectionId=cast(SectionId,it.final_section),
                           final_var_vals = cast(Optional[Dict[GlobalVarId,Any]],it.final_values), verbose=True)
    else:
        return env.evalLSM(it, verbose=True)



from src.cli import EXAMPLES_SEXPR_ROOT

from test.test_interpreter import traces
# so can run it as a library too, which respects exceptions
def main(sys_argv:List[str]):

    EXAMPLES_TO_RUN = [
        'degenerate/minimal_future-actions.l4',
        'degenerate/minimal_future-actions2.l4',
        'toy_and_teaching/monster_burger_program_only.l4',
        'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',
        'degenerate/collatz.l4',
        'serious/SAFE.l4',
    ]
    for trace in traces:
        subpath = trace[0]
        if subpath in EXAMPLES_TO_RUN:
            print("\n" + subpath)
            path = EXAMPLES_SEXPR_ROOT + subpath
            parsed = parse_file(path)
            if 'print' in sys_argv:
                print(prettySExprStr(parsed))

            assembler = L4ContractConstructor(path)
            prog : L4Contract = assembler.mk_l4contract(parsed)

            evalTrace(trace[1], prog)

    todo_once("\nASSIGN BLAME IN FUTURE OBLIG BREACH CASE")

if __name__ == '__main__':
    import sys
    main(sys.argv)


                # 'examples/hvitved_master_sales_agreement_full_with_ids.LSM': [
    #     nextTSEvent('Start'),
    #     nextTSEvent('ContractLive'),
    #     nextTSEvent('NewOrder',[50]),
    #     nextTSEvent('ContractLive'),
    #     nextTSEvent('NewOrder',[40]),
    # ],
    # 'examples_sexpr/hvitved_lease.LSM': [
    #     nextTSEvent('Move_In'),
    #     nextTSEvent('Lease_Term_Started'),
    #     nextTSEvent('EnsureApartmentReady'),
    #     nextTSEvent('Month_Started'),
    #     nextTSEvent('PayRent'),
    #     nextTSEvent('Month_Ended'),
    #     nextTSEvent('Month_Started'),
    #     nextTSEvent('RentDue'),
    #     nextTSEvent('PayRent'),
    #     nextTSEvent('Month_Ended'),
    #     nextTSEvent('Month_Started'),
    #     nextTSEvent('Request_Termination_At_Rent_Or_Before'),
    #     nextTSEvent('PayRent'),
    #     nextTSEvent('Month_Ended'),
    #     nextTSEvent('Month_Started'),
    #     nextTSEvent('PayRent'),
    #     nextTSEvent('Month_Ended'),
    #     nextTSEvent('Month_Started'),
    #     nextTSEvent('PayRent'),
    #     nextTSEvent('Request_Termination_After_Rent'),
    #     nextTSEvent('Month_Ended'),
    #     nextTSEvent('Lease_Term_Ended'),
    #     nextTSEvent('Move_Out'),
    #     nextTSEvent('Month_Started'),
    # ],