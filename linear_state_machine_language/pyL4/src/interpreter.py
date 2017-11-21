import logging

from model.Action import Action
from model.BoundVar import BoundVar, LocalVar, GlobalVar, ContractParam, ActionDeclActionParam
from model.ContractParamDec import ContractParamDec
from model.GlobalStateTransform import GlobalStateTransform
from model.GlobalStateTransformStatement import *
from model.GlobalVarDec import GlobalVarDec
from model.Literal import StringLit, Literal, DeadlineLit
from model.Term import FnApp

logging.basicConfig(
    format="[%(levelname)s] %(funcName)s: %(message)s",
    level=logging.INFO )
from typing import Tuple, Any, cast, Dict

from model.L4Contract import L4Contract

from model.Connection import ConnectionToAction, ConnectionToEnvAction
from model.Section import Section
from sexpr_to_L4Contract import L4ContractConstructor
from parse_sexpr import prettySExprStr, parse_file
from model.constants_and_defined_types import GlobalVarId, ActionId, DEADLINE_OPERATORS, DEADLINE_PREDICATES, RoleId, \
    ConnectionActionParamId, ContractParamId, SectionId, ActionParamId
from model.util import hasNotNone, dictSetOrInc, todo_once, chcast

# Nat = NewType('Nat',int)
# TimeStamp = NewType('TimeStamp',Nat)
Nat = int
TimeStamp = Nat

ActionParamValue = Union[Tuple[Any], str, int, TimeStamp]
ActionParamSubst = Dict[ActionParamId, ActionParamValue]

class Event(NamedTuple):
    action_id: ActionId
    role_id: RoleId
    timestamp: TimeStamp
    params: ActionParamSubst

class EventConsumptionResult:
    pass

class BreachResult(EventConsumptionResult):
    pass

Trace = List[Event]

FN_SYMB_INTERP = {
    '+': sum,
    '-': lambda x,y: x - y,
    '/': lambda x,y: x / y,
    '*': lambda x,y: x * y,
    '==': lambda x,y: x == y,
    '≤': lambda x,y: x <= y,
    '≥': lambda x,y: x >= y,
    '<': lambda x,y: x < y,
    '>': lambda x,y: x > y,
    'and': lambda x,y: x and y,
    'or': lambda x,y: x or y
}

DEADLINE_OP_INTERP = {
    'by': lambda curt, nextt_, args: (nextt_ <= args[0]),
    'within': lambda curt, nextt_, args: (nextt_ <= curt + args[0]),
    'nonstrictly-within': lambda curt, nextt_, args: (nextt_ <= curt + args[0]),
    'strictly-within': lambda curt, nextt_, args: (nextt_ < curt + args[0]),
    'strictly-before': lambda curt, nextt_, args: (nextt_ < args[0]),
    'nonstrictly-after-and-within': lambda curt, nextt_, args: args[0] <= nextt_ <= curt + args[1],
    'at': lambda curt, nextt_, args: (nextt_ == args[0]),
    'after': lambda curt, nextt_, args: (nextt_ == args[0]), # ??
    'after-exactly': lambda curt, nextt_, args: (nextt_ == args[0]),
}


PREFIX_FN_SYMBOLS = {'contract_start_date', 'event_start_date', 'event_start_time', 'monthStartDay', 'monthEndDay',
                     'days', #'earliest',
                     'dateplus',
                     'ifthenelse',
                     'max', 'ceil',
                     'not',
                     'enqueue', 'dequeue', 'discardTop', 'top',  # queues
                     'append', 'removeOne', 'containedIn', 'get', 'nonempty',# lists
                     'setAdd', 'setRemove', # sets
                     'tuple', 'fst', 'snd'}

class ExecEnv:
    def __init__(self, prog:L4Contract) -> None:
        self._top : L4Contract = prog
        self._varvals : Dict[LocalOrGlobalVarId, Any] = dict()
        self._var_write_cnt : Dict[LocalOrGlobalVarId, int] = dict()
        self._contract_param_vals: Dict[ContractParamId, Any] = dict()
        self._section_id: SectionId = prog.start_section
        self._timestamp = 0
        self.cur_action_param_subst_from_event : Optional[Dict[ActionParamId, ActionParamValue]]

    def cur_section(self) -> Section:
        return self._top.section(self._section_id)

    def evalLSM(self, trace:Trace, verbose=False):
        prog = self._top
        self.evalContractParamDecs(prog.contract_params)
        self.evalGlobalVarDecs(prog.global_var_decs)

        for i in range(len(trace)):
            eventi = trace[i]
            next_action_id, next_timestamp = eventi.action_id, eventi.timestamp
            
            if not(self._timestamp is None or self._timestamp <= next_timestamp):
                logging.error(f"Event timestamps must be non-decreasing, but current {self._timestamp} > next {next_timestamp}")
                continue

            action = self._top.action(next_action_id)
            if not action:
                logging.error(f"Don't recongize action id {next_action_id}")
                continue

            if not self._top.action_is_sometimes_available_from_section(self._section_id, next_action_id):
                logging.error(f"Cannot *ever* do action {next_action_id} from section {self._section_id} (no {next_action_id}-connection exists).")
                continue

            # result = self.attempt_consume_next_event(eventi):
            # if result.successful:
            if self.action_available_from_cur_section(next_action_id, next_timestamp):
                if verbose:
                    sec_pad = ' '*(prog.max_section_id_len-len(self._section_id))
                    act_pad = ' '*(prog.max_action_id_len-len(next_action_id))
                    print(f"{self._section_id}{sec_pad} --{next_action_id}-->{act_pad} {action.dest_section_id}")
                self.apply_action(action, eventi.params, next_timestamp)
                continue
            else:
                logging.error(f"{next_action_id}-connection exists in {self._section_id} but is disabled.")

            logging.error("Stopping evaluation")
            return

    # def attempt_consume_next_event(self, event:Event) -> EventConsumptionResult:
    #     active_strong_obligs : List[ConnectionToAction] = list()
    #     active_weak_obligs : List[ConnectionToAction] = list()
    #     active_permissions : List[ConnectionToAction] = list()
    #     active_env_connection : List[ConnectionToEnvAction] = list()
    #
    #     for c in self.cur_section().connections():
    #         enabled = True
    #         if c.enabled_guard:
    #             if not chcast(bool, self.evalTerm(c.enabled_guard, None)):
    #                 enabled = False
    #
    #         if enabled:
    #             if isinstance(c,ConnectionToEnvAction):
    #                 active_env_connection.append(c)
    #             else:
    #                 assert isinstance(c, ConnectionToAction)
    #                 if c.deontic_modality







    def action_available_from_cur_section(self, actionid:ActionId, next_timestamp:TimeStamp) -> bool:
        todo_once("this needs to depend on action parameters")
        for c in self.cur_section().connections():
            todo_once("check enabled guard")
            if isinstance(c, ConnectionToAction) or isinstance(c, ConnectionToEnvAction):
                if c.action_id != actionid:
                    continue
            else:
                raise NotImplementedError

            deadline_ok = self.evalDeadlineClause(c.deadline_clause, next_timestamp)
            # print("deadline_ok", deadline_ok)
            if deadline_ok:
                return True
        return False

    def apply_action(self, action:Action, params:ActionParamSubst, next_timestamp: TimeStamp):
        if action.global_state_transform:
            self.cur_action_param_subst_from_event = params
            self.evalCodeBlock(action.global_state_transform)
            self.cur_action_param_subst_from_event = None

        self._section_id = action.dest_section_id
        self._timestamp = next_timestamp


    def evalGlobalVarDecs(self, decs : Dict[GlobalVarId, GlobalVarDec]):
        for (var,dec) in cast(Dict[LocalOrGlobalVarId, GlobalVarDec],decs).items():
            if dec.initval is None:
                self._varvals[var] = None
                print(f"Global var {var} has no initial value.")
                dictSetOrInc(self._var_write_cnt, var, init=0)
            else:
                self._varvals[var] = self.evalTerm(dec.initval, 0)
                print(f"Global var {var} has initial value {self._varvals[var]}.")
                dictSetOrInc(self._var_write_cnt, var, init=1)
            # print('evalGlobalVarDecs: ', var, dec, self._var_write_cnt[var])


    def evalContractParamDecs(self, decs : Dict[ContractParamId, ContractParamDec]):
        for (name,dec) in decs.items():
            # print(dec)
            if not(dec.value_expr is None):
                self._contract_param_vals[name] = self.evalTerm(dec.value_expr, 0)
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
            value = self.evalTerm(stmt.value_expr, self._timestamp)
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
            value = self.evalTerm(stmt.value_expr, self._timestamp)
            self._varvals[stmt.varname] = value

        elif isinstance(stmt, (IncrementStatement,DecrementStatement,TimesEqualsStatement)):
            vardec = self._top.varDecObj(stmt.varname)
            # todo: use vardec for runtime type checking
            value = self.evalTerm(stmt.value_expr, self._timestamp)
            if not stmt.varname in self._varvals:
                logging.error(stmt.varname + " should be in the execution environment, but isn't.")
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
            print("conjecture " + str(stmt))

        else:
            raise NotImplementedError("Unhandled GlobalStateTransformStatement: " + str(stmt))

    def evalTerm(self, term:Term, next_timestamp:Optional[TimeStamp]) -> Any:
        assert term is not None
        assert next_timestamp is not None
        # print('evalTerm: ', term)
        if isinstance(term, FnApp):
            return self.evalFnApp(term, next_timestamp)
        elif isinstance(term, DeadlineLit):
            # print("DeadlineLit: ", term)
            return next_timestamp == self._timestamp
            # logging.error("got here finally?")
        elif isinstance(term, Literal):
            return term.lit
        elif isinstance(term, LocalVar):
            assert hasNotNone(self._varvals, cast(LocalOrGlobalVarId,term.name)), term.name
            return self._varvals[cast(LocalOrGlobalVarId,term.name)]
        elif isinstance(term, GlobalVar):
            # print(self._contract_param_vals)
            # print(self._varvals)
            assert hasNotNone(self._varvals, cast(LocalOrGlobalVarId, term.name)), "Global var " + term.name + " should have a value but doesn't."
            return self._varvals[cast(LocalOrGlobalVarId, term.name)]
        elif isinstance(term, ContractParam):
            # print("ContractParam case of evalTerm: ", term)
            # print(self._contract_param_vals[term.name], type(self._contract_param_vals[term.name]))
            assert hasNotNone(self._contract_param_vals, term.name), term.name
            return self._contract_param_vals[term.name]
        elif isinstance(term,ActionDeclActionParam):
            if self.cur_action_param_subst_from_event:
                if term.name in self.cur_action_param_subst_from_event:
                    return self.cur_action_param_subst_from_event[cast(ActionParamId, term.name)]
                todo_once("HACK!")
                if "_" + term.name in self.cur_action_param_subst_from_event:
                    return self.cur_action_param_subst_from_event[cast(ActionParamId, '_'+term.name)]

            logging.error("Trying to evaluate ActionDeclActionParam but didn't find it in the execution context.")
        else:
            logging.error("evalTerm unhandled case for: " + str(term))
            return term

    def evalDeadlineClause(self, deadline_clause:Term, next_timestamp:TimeStamp) -> bool:
        assert deadline_clause is not None
        # return True
        rv = chcast(bool, self.evalTerm(deadline_clause, next_timestamp))
        assert rv is not None
        # print('evalDeadlineClause: ', rv)
        return rv

    def evalFnApp(self, fnapp:FnApp, next_timestamp: Optional[TimeStamp]) -> Any:
        fn = fnapp.head
        # print("the fnapp: ", str(fnapp), " with args ", fnapp.args)
        evaluated_args = tuple(self.evalTerm(x,next_timestamp) for x in fnapp.args)
        if fn in FN_SYMB_INTERP:
            return cast(Any,FN_SYMB_INTERP[fn])(evaluated_args)
        elif fn == "unitsAfterEntrance":
            assert len(evaluated_args) == 1
            assert next_timestamp is not None
            return evaluated_args[0] + next_timestamp
        elif fn in DEADLINE_OPERATORS or fn in DEADLINE_PREDICATES:
            assert next_timestamp is not None
            if fn in DEADLINE_OP_INTERP:
                return cast(Any, DEADLINE_OP_INTERP[fn])(self._timestamp, next_timestamp, evaluated_args)
            else:
                logging.error("Unhandled deadline fn symbol: " + fn)
        else:
            logging.error("Unhandled fn symbol: " + fn)
        return 0


def evalLSM(trace:Trace, prog:L4Contract):
    env = ExecEnv(prog)
    return env.evalLSM(trace, verbose=True)


timestamp = 0
def nextTSEvent(action_id:str, role_id:str, params=Dict[str, ActionParamValue]):
    global timestamp
    params = params or []
    timestamp += 1
    return Event(action_id=castid(ActionId,action_id), role_id=castid(RoleId,role_id), timestamp=timestamp,
                 params= cast(ActionParamSubst, params))

def sameTSEvent(action_id:str, role_id:str, params=Dict[str, ActionParamValue]):
    params = params or []
    return Event(action_id=castid(ActionId,action_id), role_id=castid(RoleId,role_id), timestamp=timestamp,
                 params=cast(ActionParamSubst, params))

def event(action_id:str, role_id:str, timestamp:int, params=Dict[str, ActionParamValue]):
    return Event(action_id=castid(ActionId, action_id), role_id=castid(RoleId, role_id), timestamp=timestamp,
                 params=cast(ActionParamSubst, params))

from cli import EXAMPLES_SEXPR_ROOT

traces = {
    'toy_and_teaching/monster_burger_program_only.l4': [
        # start section implicit
        nextTSEvent('RequestCookMB', 'Challenger'),
        nextTSEvent('ServeMB', 'Restaurant'),
        sameTSEvent('EnterEatingMB', 'Env'),
        nextTSEvent('AnnounceMBFinished', 'Challenger'),
        nextTSEvent('CheckFinishedClaim', 'Restaurant'),
        sameTSEvent('RejectFinishedClaim', 'Restaurant'),
        sameTSEvent('EnterEatingMB', 'Env'),
        nextTSEvent('AnnounceMBFinished','Challenger'),
        nextTSEvent('CheckFinishedClaim', 'Restaurant'),
        sameTSEvent('VerifyFinishedClaim', 'Restaurant')
    ],


    'from_academic_lit/hvitved_instalment_sale--simplified_time.l4': [
        # start section implicit
        event('PayInstallment', 'Buyer', 30, {'amount':501}),
        event('PayInstallment', 'Buyer', 60, {'amount':501} )

    ],

}



if __name__ == '__main__':
    import sys

    EXAMPLES_TO_RUN = [
        'toy_and_teaching/monster_burger_program_only.l4',
        'from_academic_lit/hvitved_instalment_sale--simplified_time.l4'
    ]
    for subpath in EXAMPLES_TO_RUN:
        print("\n" + subpath)
        path = EXAMPLES_SEXPR_ROOT + subpath
        parsed = parse_file(path)
        if 'print' in sys.argv:
            print(prettySExprStr(parsed))

        assembler = L4ContractConstructor(path)
        prog : L4Contract = assembler.l4contract(parsed)

        evalLSM(traces[subpath], prog)



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