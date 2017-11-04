import logging
logging.basicConfig(
    format="[%(levelname)s] %(funcName)s: %(message)s",
    level=logging.INFO )

from typing import NamedTuple, NewType, Set, Union, Tuple, List, Any, Dict
from LSMTop import LSMTop
from LSMStatements import *
from LSMEventState import EventState
from compileLSM import Assemble, parse_file
from parse_sexpr import prettySExprStr
from constants_and_defined_types import GlobalVarId
from util import hasNotNone, dictSetOrInc

# Nat = NewType('Nat',int)
# TimeStamp = NewType('TimeStamp',Nat)
Nat = int
TimeStamp = Nat

EventParamValue = Union[Tuple[Any],str,int,TimeStamp]

# class Event(NamedTuple):
#     name: str 
#     TimeStamp: TimeStamp
#     params: List[EventParamValue]
class Event(NamedTuple):
    name: str
    timestamp: TimeStamp
    params: List[EventParamValue]

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
    def __init__(self, prog:LSMTop) -> None:
        self._top : LSMTop = prog
        self._varvals : Dict[str, Any] = dict()
        self._var_write_cnt : Dict[str, int] = dict()
        self._contract_param_vals: Dict[str, Any] = dict()
        self._state : str = None #prog.formal_contract.start_state
        self._timestamp : int = None

    def evalLSM(self, trace:Trace):
        self.evalContractParamDecs(self._top.contract_params)
        self.evalGlobalVarDecs(self._top.global_var_decs)
        print(self._top.global_var_decs)

        print('Globals\n\t', self._varvals)

        for i in range(len(trace)):
            eventi = trace[i]
            next_state_name, next_timestamp = eventi.name, eventi.timestamp
            
            if not(self._timestamp is None or self._timestamp < next_timestamp):
                logging.error(f"Event timestamps must be strictly increasing: {next_timestamp} !> {self._timestamp}")
                continue
            if not self._top.can_transition(self._state, next_state_name):
                logging.error(f"Cannot ever transition from {self._state} to {next_state_name}")
                continue
            # if prog.can_transition(self._state, next_state_name):
            #     self._timestamp = next_timestamp
            #     self._state = next_state_name
            #     self.evalCodeBlock(eventi)
            if self.can_transition(next_state_name, next_timestamp):
                self._timestamp = next_timestamp
                self._state = next_state_name
                self.evalCodeBlock(eventi)

            # print('Globals\n\t', self._varvals)

    def ensureCanTransition(self, next_state_name:str, next_timestamp:int) -> None:
        if not (self._timestamp is None or self._timestamp < next_timestamp):
            logging.error(f"Event timestamps must be strictly increasing: {next_timestamp} !> {self._timestamp}")
            return
        if not self._top.can_transition(self._state, next_state_name):
            logging.error(f"Cannot ever transition from {self._state} to {next_state_name}")
            return
        self.evalCodeBlock(eventi)
        if next_timestamp < self.evalDeadline()

    def evalGlobalVarDecs(self, decs : Dict[GlobalVarId, GlobalVarDec]):
        for (var,dec) in decs.items():
            if dec.initval is None:
                self._varvals[var] = None
            else:
                self._varvals[var] = self.evalTerm(dec.initval)
                dictSetOrInc(self._var_write_cnt, var, init=1)
            print('evalGlobalVarDecs: ', var, dec, self._var_write_cnt[var])


    def evalContractParamDecs(self, decs : Dict[str, ContractParamDec]):
        for (name,dec) in decs.items():
            print(dec)
            if not(dec.value_expr is None):
                self._contract_param_vals[name] = self.evalTerm(dec.value_expr)
            else:
                self._contract_param_vals[name] = None

    def evalCodeBlock(self, event:Event):
        eventstate = self._top.estate(event.name)
        # print(eventstate)
        if not eventstate.code_block:
            return
        for statement in eventstate.code_block.statements:
            self.evalStatement(statement)

    def evalStatement(self, stmt:CodeBlockStatement):

        if isinstance(stmt, LocalVarDec):
            assert self._top.varDecObj(stmt.varname) is None
            value = self.evalTerm(stmt.value_expr)
            self._varvals[stmt.varname] = value
        elif isinstance(stmt, VarAssignStatement):
            vardec = self._top.varDecObj(stmt.varname)
            print(self._var_write_cnt[stmt.varname])
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

    def evalFnApp(self, fnapp:FnApp) -> Any:
        fn = fnapp.head
        args = (self.evalTerm(x) for x in fnapp.args)
        if fn in FN_SYMB_INTERP:
            return FN_SYMB_INTERP[fn](args)
        return 0


def evalLSM(trace:Trace, prog:LSMTop):
    env = ExecEnv(prog)
    return env.evalLSM(trace)


timestamp = 0
def event(name,params=None):
    global timestamp
    timestamp += 1
    params = params or []
    return Event(name=name, timestamp=timestamp, params=params)


traces = {
    'examples/hvitved_master_sales_agreement_full_with_ids.LSM': [
        event('Start'),
        event('ContractLive'),
        event('NewOrder',[50]),
        event('ContractLive'),
        event('NewOrder',[40]),
    ],
    'examples/hvitved_lease.LSM': [
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

    ]
}



if __name__ == '__main__':
    import sys

    EXAMPLES = ['examples/hvitved_lease.LSM']
    for path in EXAMPLES:
        parsed = parse_file(path)
        if 'print' in sys.argv:
            print(prettySExprStr(parsed))
        assembler = Assemble(path)
        prog : LSMTop = assembler.top(parsed)
        evalLSM(traces[path], prog)

