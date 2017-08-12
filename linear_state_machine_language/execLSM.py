import logging
logging.basicConfig(
    format="[%(levelname)s] %(funcName)s: %(message)s",
    level=logging.INFO )

from typing import NamedTuple, NewType, Set, Union, Tuple, List, Any, Dict
from LSMTop import LSMTop
from LSMStatements import *
from compileLSM import Assemble, parse_file
from parse_sexpr import pretty
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
Event = NamedTuple('Event', [ ('name',str), ('timestamp',TimeStamp), ('params', List[EventParamValue])] )    

Trace = List[Event]

class ExecEnv:
    def __init__(self, prog:LSMTop) -> None:
        self._top : LSMTop = prog
        self._gvarvals : Dict[GlobalVarId, Any] = dict()
        self._state : str = None #prog.formal_contract.start_state
        self._timestamp : int = None

    def evalLSM(self, trace:Trace):
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

    def evalCodeBlock(self, event:Event):
        eventstate = self._top.estate(event.name)
        print(eventstate)
        if not eventstate.code_block:
            return
        for statement in eventstate.code_block.statements:
            self.evalStatement(statement)

    def evalStatement(self, stmt:CodeBlockStatement):
        if isinstance(stmt, VarAssignStatement):
            varobj = self._top.varObj(stmt.varname)
            value = self.evalTerm(stmt.value_expr)
        elif isinstance(stmt, IncrementStatement):
            varobj = self._top.varObj(stmt.varname)
            value = self.evalTerm(stmt.value_expr)
        elif isinstance(stmt, DecrementStatement):
            varobj = self._top.varObj(stmt.varname)
            value = self.evalTerm(stmt.value_expr)

    def evalTerm(self, term:Term) -> Any:
        pass


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
            print(pretty(parsed))
        assembler = Assemble(path)
        prog : LSMTop = assembler.top(parsed)
        evalLSM(traces[path], prog)

