import math
from datetime import timedelta
from typing import Iterable, NamedTuple

from src.constants_and_defined_types import RoleId
from src.interpreter.timedelta_map import tdmapDelete, tdmapSet, tdmapHas, tdmapTimeDeltaGEQ, tdmapTimeDeltaLT
from src.model.ActionRule import PartlyInstantiatedPartyFutureActionRule
from src.model.EventsAndTraces import Event
from src.model.GlobalStateTransformStatement import *


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


ENV_VAR_INTERP = {
    'event_role': lambda execenv: execenv.cur_event.role_id,
    'contractStart_dt': lambda execenv: execenv.start_datetime(),
    'contractStart_td': lambda execenv: execenv.datetime2delta(execenv.start_datetime),
    'event_td':  lambda execenv: execenv.cur_event_delta(),
    'next_event_td':  lambda execenv: execenv.cur_event_delta(),
    'future_event_td':  lambda execenv: execenv.cur_event_delta(),
    'sectionEntrance_td': lambda execenv: execenv.last_section_entrance_delta
}

FN_SYMB_INTERP = {
    'cast': lambda x,y: y,
    # '+': lambda *args: sum(args),  # doesn't work with timestamp
    '+': lambda x,y: x + y,
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
    'and*': lambda *x: all([y for y in x]),
    'or': lambda x,y: x or y,
    'not': lambda x: not x,
    'days': lambda x: timedelta(days=x),
    'round': round,
    'ceil': math.ceil,

    # 'tuple' : lambda x,y: (x,y),
    'tuple' : lambda *x: tuple([*x]),
    'tupleGet' : lambda t,i: t[i],

    'mapSet' : tdmapSet,
    'mapDelete' : tdmapDelete,
    'mapHas' : tdmapHas,
    'tdGEQ' : tdmapTimeDeltaGEQ,
    'tdLT' : tdmapTimeDeltaLT,
    'emptyTDMap' : lambda: tuple(),
    'nonempty' : lambda x: len(x) > 0,
    'empty' : lambda x: len(x) == 0
}

def event_to_action_str(event:Event):
    if event.params_by_abap_name:
        params_str = ", ".join([f"{key}: {event.params_by_abap_name[key]}" for key in event.params_by_abap_name.keys()])
        return f"{event.action_id}({params_str})"
    else:
        return f"{event.action_id}"

class EvalError(Exception):
    pass
