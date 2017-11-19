import logging

from model.Action import Action
from model.BoundVar import BoundVar, LocalVar, GlobalVar, ContractParam
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
from model.constants_and_defined_types import GlobalVarId, ActionId, DEADLINE_OPERATORS
from model.util import hasNotNone, dictSetOrInc, todo_once

# Nat = NewType('Nat',int)
# TimeStamp = NewType('TimeStamp',Nat)
Nat = int
TimeStamp = Nat

ActionParamsValue = Union[Tuple[Any], str, int, TimeStamp]

class Event(NamedTuple):
    action_id: str
    timestamp: TimeStamp
    params: List[ActionParamsValue]

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
    'after': lambda curt, nextt_, args: (nextt_ == args[0]),
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
        self._varvals : Dict[str, Any] = dict()
        self._var_write_cnt : Dict[str, int] = dict()
        self._contract_param_vals: Dict[str, Any] = dict()
        self._section_id: str = prog.start_section
        self._timestamp = 0

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
                logging.error(f"Event timestamps must be increasing: {next_timestamp} < {self._timestamp}")
                continue

            action = self._top.action(next_action_id)
            if not action:
                logging.error(f"Don't recongize action id {next_action_id}")
                continue

            if not self._top.action_is_sometimes_available_from_section(self._section_id, next_action_id):
                logging.error(f"Cannot *ever* do action {next_action_id} from section {self._section_id} (no {next_action_id}-connection exists).")
                continue

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

    def action_available_from_cur_section(self, actionid:ActionId, next_timestamp:TimeStamp) -> bool:
        todo_once("this needs to depend on action parameters")
        for c in self.cur_section().connections():
            deadline_ok = self.evalDeadlineClause( cast(Any,c).deadline_clause, next_timestamp )
            todo_once("check enabled guard")
            if not deadline_ok:
                continue
            if isinstance(c, ConnectionToAction) or isinstance(c, ConnectionToEnvAction):
                if c.action_id == actionid:
                    return True
            else:
                raise NotImplementedError
        return False

    def apply_action(self, action:Action, params:ActionParamsValue, next_timestamp: TimeStamp):
        if action.global_state_transform:
            self.evalCodeBlock(action.global_state_transform)
        self._section_id = action.dest_section_id
        self._timestamp = next_timestamp


    def evalGlobalVarDecs(self, decs : Dict[GlobalVarId, GlobalVarDec]):
        for (var,dec) in decs.items():
            if dec.initval is None:
                self._varvals[var] = None
                dictSetOrInc(self._var_write_cnt, var, init=0)
            else:
                self._varvals[var] = self.evalTerm(dec.initval, 0)
                dictSetOrInc(self._var_write_cnt, var, init=1)
            # print('evalGlobalVarDecs: ', var, dec, self._var_write_cnt[var])


    def evalContractParamDecs(self, decs : Dict[str, ContractParamDec]):
        for (name,dec) in decs.items():
            # print(dec)
            if not(dec.value_expr is None):
                self._contract_param_vals[name] = self.evalTerm(dec.value_expr, 0)
            else:
                self._contract_param_vals[name] = None

    def evalCodeBlock(self, transform:GlobalStateTransform):
        # print("\nevalCodeBlock\n")
        # action = self._top.action(event.action_id)
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
            # else:
            #     raise NotImplementedError("Non-GlobalVar assign statement unhandled")
            # print('evalStatement: ' + str(stmt))
            dictSetOrInc(self._var_write_cnt, stmt.varname, init=1)
            # todo: use vardec for runtime type checking
            value = self.evalTerm(stmt.value_expr, self._timestamp)
            self._varvals[stmt.varname] = value
        elif isinstance(stmt, IncrementStatement):
            vardec = self._top.varDecObj(stmt.varname)
            # todo: use vardec for runtime type checking
            value = self.evalTerm(stmt.value_expr, self._timestamp)
            assert self._varvals[stmt.varname] is not None, "Tried to increment uninitialized variable `" + stmt.varname + "`"
            self._varvals[stmt.varname] = self._varvals[stmt.varname] + int(value)
        elif isinstance(stmt, DecrementStatement):
            varobj = self._top.varDecObj(stmt.varname)
            value = self.evalTerm(stmt.value_expr, self._timestamp)
            self._varvals[stmt.varname] = self._varvals[stmt.varname] - int(value)
        else:
            logging.error("Unhandled GlobalStateTransformStatement: " + str(stmt))

    def evalTerm(self, term:Term, next_timestamp:TimeStamp) -> Any:
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
            assert hasNotNone(self._varvals, term.name), term.name
            return self._varvals[term.name]
        elif isinstance(term, GlobalVar):
            # print(self._contract_param_vals)
            # print(self._varvals)
            assert hasNotNone(self._varvals, term.name), "Global var " + term.name + " should have a value but doesn't."
            return self._varvals[term.name]
        elif isinstance(term, ContractParam):
            # print("ContractParam case of evalTerm: ", term)
            # print(self._contract_param_vals[term.name], type(self._contract_param_vals[term.name]))
            assert hasNotNone(self._contract_param_vals, term.name), term.name
            return self._contract_param_vals[term.name]
        else:
            logging.error("evalTerm unhandled case for: " + str(term))
            return term

    def evalDeadlineClause(self, deadline_clause:Term, next_timestamp:TimeStamp) -> bool:
        assert deadline_clause is not None
        # return True
        rv = cast(bool, self.evalTerm(deadline_clause, next_timestamp))
        assert rv is not None
        # print('evalDeadlineClause: ', rv)
        return rv

    def evalFnApp(self, fnapp:FnApp, next_timestamp:TimeStamp) -> Any:
        fn = fnapp.head
        # print("the fnapp: ", str(fnapp), " with args ", fnapp.args)
        evaluated_args = tuple(self.evalTerm(x,next_timestamp) for x in fnapp.args)
        if fn in FN_SYMB_INTERP:
            return cast(Any,FN_SYMB_INTERP[fn])(evaluated_args)
        elif fn == "unitsAfterEntrance":
            assert len(evaluated_args) == 1
            return evaluated_args[0] + next_timestamp
        elif fn in DEADLINE_OPERATORS:
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
def event(action_id:ActionId, params=ActionParamsValue, same_timestamp=False):
    global timestamp
    params = params or []
    if not same_timestamp:
        timestamp += 1
    return Event(action_id=action_id, timestamp=timestamp, params=params)

traces = {
    'examples/hvitved_master_sales_agreement_full_with_ids.LSM': [
        event('Start'),
        event('ContractLive'),
        event('NewOrder',[50]),
        event('ContractLive'),
        event('NewOrder',[40]),
    ],
    'examples_sexpr/hvitved_lease.LSM': [
        event('Move_In'),
        event('Lease_Term_Started'),
        event('EnsureApartmentReady'),
        event('Month_Started'),
        event('PayRent'),
        event('Month_Ended'),
        event('Month_Started'),
        event('RentDue'),
        event('PayRent'),
        event('Month_Ended'),
        event('Month_Started'),
        event('Request_Termination_At_Rent_Or_Before'),
        event('PayRent'),
        event('Month_Ended'),
        event('Month_Started'),
        event('PayRent'),
        event('Month_Ended'),
        event('Month_Started'),
        event('PayRent'),
        event('Request_Termination_After_Rent'),
        event('Month_Ended'),
        event('Lease_Term_Ended'),
        event('Move_Out'),
        event('Month_Started'),
    ],
    'examples_sexpr/monster_burger_program_only.l4': [
        # event('MonsterBurgerUncooked'), # implicit
        event('RequestCookMB'),
        event('ServeMB'),
        event('EnterEatingMB', None, True),
        event('AnnounceMBFinished'),
        event('CheckFinishedClaim'),
        event('RejectFinishedClaim', None, False),
        event('EnterEatingMB', None, True),
        event('AnnounceMBFinished'),
        event('CheckFinishedClaim'),
        event('VerifyFinishedClaim', None, True)
    ]
}



if __name__ == '__main__':
    import sys

    EXAMPLES = ['examples_sexpr/monster_burger_program_only.l4']
    for path in EXAMPLES:
        parsed = parse_file(path)
        if 'print' in sys.argv:
            print(prettySExprStr(parsed))
        assembler = L4ContractConstructor(path)
        prog : L4Contract = assembler.l4contract(parsed)
        evalLSM(traces[path], prog)

