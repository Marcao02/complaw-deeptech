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
        self._state : str = None #prog.formal_contract.start_state
        self._timestamp : int = None

    def evalLSM(self, trace:Trace):
        self.evalGlobalVarDecs(self._top.global_var_decs)
        print('Globals\n\t', self._varvals)

        for i in range(len(trace)):
            event = trace[i]
            next_state_name, next_timestamp = event.name, event.timestamp
            
            if self._timestamp is None or self._timestamp < next_timestamp:
                self._timestamp = next_timestamp
            else:
                logging.error(f"Event timestamps must be strictly increasing: {next_timestamp} !> {self._timestamp}")

            if prog.can_transition(self._state, next_state_name):
                self._state = next_state_name
                self.evalCodeBlock(event)
            else:
                logging.error(f"Cannot ever transition from {self._state} to {next_state_name}")

            print('Globals\n\t', self._varvals)

    def evalGlobalVarDecs(self, decs : Dict[GlobalVarId, GlobalVarDec]):
        for (var,dec) in decs.items():
            print(dec)
            if not(dec.initval is None):
                self._varvals[var] = self.evalTerm(dec.initval)
            else:
                self._varvals[var] = None

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
            varobj = self._top.varDecObj(stmt.varname)
            value = self.evalTerm(stmt.value_expr)
            self._varvals[stmt.varname] = value
        elif isinstance(stmt, IncrementStatement):
            varobj = self._top.varDecObj(stmt.varname)
            value = self.evalTerm(stmt.value_expr)
            assert self._varvals[stmt.varname] is not None, "Tried to increment uninitialized variable `" + stmt.varname + "`"
            self._varvals[stmt.varname] = self._varvals[stmt.varname] + int(value)
        elif isinstance(stmt, DecrementStatement):
            varobj = self._top.varDecObj(stmt.varname)
            value = self.evalTerm(stmt.value_expr)
            self._varvals[stmt.varname] = self._varvals[stmt.varname] - int(value)

    def evalTerm(self, term:Term) -> Any:
        print(term)
        if isinstance(term, FnApp):
            return self.evalFnApp(term)
        elif isinstance(term, StringLit):
            return term.lit
        elif isinstance(term, LocalVar):
            return self._varvals[term.name]
        elif isinstance(term, GlobalVar):
            return self._varvals[term.name]
        elif isinstance(term, Atom):
            return term.atom
        else:
            return term

    def evalFnApp(self, fnapp:FnApp) -> Any:
        fn = fnapp.head
        args = (self.evalTerm(x) for x in fnapp.args)
        print([str(x) for x in args])
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

