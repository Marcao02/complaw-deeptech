import logging



logging.basicConfig(
    format="[%(levelname)s] %(funcName)s: %(message)s",
    level=logging.INFO )
from typing import NamedTuple, NewType, Set, Union, Tuple, List, Any, Dict, cast

from model.L4Contract import L4Contract
from model.statements import *
from model.Connection import Connection, ConnectionToAction, ConnectionToEnvAction
from model.Section import Section
from sexpr_to_L4Contract import L4ContractConstructor
from parse_sexpr import prettySExprStr, parse_file
from model.constants_and_defined_types import GlobalVarId
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
    'or': lambda x,y: x or y }

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
            
            if not(self._timestamp is None or self._timestamp < next_timestamp):
                logging.error(f"Event timestamps must be strictly increasing: {next_timestamp} !> {self._timestamp}")
                continue
            action = self._top.action(next_action_id)
            if not action:
                logging.error(f"Don't recongize action id {next_action_id}")
                continue
            if not self._top.action_sometimes_available_from_section(self._section_id, next_action_id):
                logging.error(f"Cannot *ever* do action {next_action_id} from section {self._section_id}")
                continue
            if self.action_available_from_cur_section(next_action_id):
                if verbose:
                    sec_pad = ' '*(prog.max_section_id_len-len(self._section_id))
                    act_pad = ' '*(prog.max_action_id_len-len(next_action_id))
                    print(f"{self._section_id}{sec_pad} --{next_action_id}-->{act_pad} {action.dest_section_id}")
                self._section_id = action.dest_section_id
                self._timestamp = next_timestamp
                continue

    def action_available_from_cur_section(self, actionid:ActionId) -> bool:
        for c in self.cur_section().connections():
            deadline_ok = self.evalDeadlineClause( cast(Any,c).deadline_clause )
            todo_once("check enabled guard")
            if not deadline_ok:
                continue
            if isinstance(c, ConnectionToAction) or isinstance(c, ConnectionToEnvAction):
                if c.action_id == actionid:
                    return True
            else:
                raise NotImplementedError
        return False


    def evalGlobalVarDecs(self, decs : Dict[GlobalVarId, GlobalVarDec]):
        for (var,dec) in decs.items():
            if dec.initval is None:
                self._varvals[var] = None
                dictSetOrInc(self._var_write_cnt, var, init=0)
            else:
                self._varvals[var] = self.evalTerm(dec.initval)
                dictSetOrInc(self._var_write_cnt, var, init=1)
            # print('evalGlobalVarDecs: ', var, dec, self._var_write_cnt[var])


    def evalContractParamDecs(self, decs : Dict[str, ContractParamDec]):
        for (name,dec) in decs.items():
            # print(dec)
            if not(dec.value_expr is None):
                self._contract_param_vals[name] = self.evalTerm(dec.value_expr)
            else:
                self._contract_param_vals[name] = None

    def evalCodeBlock(self, event:Event):
        action = self._top.action(event.action_id)
        if not action.global_state_transform:
            return
        for statement in action.global_state_transform.statements:
            self.evalStatement(statement)

    def evalStatement(self, stmt:CodeBlockStatement):

        if isinstance(stmt, LocalVarDec):
            assert self._top.varDecObj(stmt.varname) is None
            value = self.evalTerm(stmt.value_expr)
            self._varvals[stmt.varname] = value
        elif isinstance(stmt, VarAssignStatement):
            vardec = self._top.varDecObj(stmt.varname)
            # print(self._var_write_cnt[stmt.varname])
            if isinstance(vardec, GlobalVarDec):
                if vardec.isWriteOnceMore() and self._var_write_cnt[stmt.varname] >= 2:
                    raise Exception(f"Attempt to write twice more (after init) to writeOnceMore variable `{stmt.varname}`")
            dictSetOrInc(self._var_write_cnt, stmt.varname, init=1)
            # todo: use vardec for runtime type checking
            value = self.evalTerm(stmt.value_expr)
            self._varvals[stmt.varname] = value
        elif isinstance(stmt, IncrementStatement):
            vardec = self._top.varDecObj(stmt.varname)
            # todo: use vardec for runtime type checking
            value = self.evalTerm(stmt.value_expr)
            assert self._varvals[stmt.varname] is not None, "Tried to increment uninitialized variable `" + stmt.varname + "`"
            self._varvals[stmt.varname] = self._varvals[stmt.varname] + int(value)
        elif isinstance(stmt, DecrementStatement):
            varobj = self._top.varDecObj(stmt.varname)
            value = self.evalTerm(stmt.value_expr)
            self._varvals[stmt.varname] = self._varvals[stmt.varname] - int(value)

    def evalTerm(self, term:Term) -> Any:
        # print('evalTerm: ', term)
        if isinstance(term, FnApp):
            return self.evalFnApp(term)
        elif isinstance(term, StringLit):
            return term.lit
        elif isinstance(term, LocalVar):
            assert hasNotNone(self._varvals, term.name), term.name
            return self._varvals[term.name]
        elif isinstance(term, GlobalVar):
            assert hasNotNone(self._varvals, term.name), term.name
            return self._varvals[term.name]
        elif isinstance(term, ContractParam):
            assert hasNotNone(self._contract_param_vals, term.name), term.name
            return self._contract_param_vals[term.name]
        elif isinstance(term, Atom):
            raise Exception(term)
            return term.atom
        else:
            return term

    def evalDeadlineClause(self, deadline_clause:Term) -> bool:
        todo_once("evalDeadlineClause")
        return True

    def evalFnApp(self, fnapp:FnApp) -> Any:
        fn = fnapp.head
        args = (self.evalTerm(x) for x in fnapp.args)
        if fn in FN_SYMB_INTERP:
            return cast(Any,FN_SYMB_INTERP[fn])(args)
        return 0


def evalLSM(trace:Trace, prog:L4Contract):
    env = ExecEnv(prog)
    return env.evalLSM(trace, verbose=True)


timestamp = 0
def event(action_id:ActionId, params=ActionParamsValue):
    global timestamp
    timestamp += 1
    params = params or []
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
    'examples_sexpr/monster_burger.l4': [
        # event('MonsterBurgerUncooked'), # implicit
        event('RequestCookMB'),
        event('ServeMB'),
        event('EnterEatingMB'),
        event('AnnounceMBFinished'),
        event('CheckFinishedClaim'),
        event('RejectFinishedClaim'),
        event('EnterEatingMB'),
        event('AnnounceMBFinished'),
        event('CheckFinishedClaim'),
        event('VerifyFinishedClaim'),
    ]
}



if __name__ == '__main__':
    import sys

    EXAMPLES = ['examples_sexpr/monster_burger.l4']
    for path in EXAMPLES:
        parsed = parse_file(path)
        if 'print' in sys.argv:
            print(prettySExprStr(parsed))
        assembler = L4ContractConstructor(path)
        prog : L4Contract = assembler.l4contract(parsed)
        evalLSM(traces[path], prog)

