import math
from datetime import timedelta
from typing import Iterable, NamedTuple

from src.constants_and_defined_types import RoleId
from src.interpreter.timedelta_map import tdmapSet, tdmapTimeDeltaGEQ, tdmapTimeDeltaLT, \
    tdmapHasItemExpiredBefore, tdmapMinValue, colHas, deleteFromCollection, tdmapAdd, tdmapTimeDeltaLEQ
from src.model.EventsAndTraces import Event
from src.model.Statement import *


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

    'last_event_td':  lambda execenv: execenv.cur_event_delta(),
    'next_event_td':  lambda execenv: execenv.cur_event_delta(),
    'future_event_td':  lambda execenv: execenv.cur_event_delta(),

    'last_situation_td': lambda execenv: execenv.last_situation_entrance_delta
}

def and_eval(*args:Any) -> bool:
    if len(args) == 1 and isinstance(args[0],Iterable):
        return all((y for y in args[0]))
    else:
        return all((y for y in args))


def or_eval(*args: Any) -> bool:
    if len(args) == 1 and isinstance(args[0], Iterable):
        return not all((not y for y in args[0]))
    else:
        return not all((not y for y in args))


FN_SYMB_INTERP = {
    'cast': lambda x,y: y,
    'check': lambda x,y: y,
    'units': lambda x,y: y,
    'trust': lambda x,y: y,

    # '+': lambda *args: sum(args),  # doesn't work with timestamp
    '+': lambda x,y: x + y,
    '-': lambda x,y: x - y,
    '/': lambda x,y: x / y,

    'fraction-of-sum': lambda x,y: x / (x+y),
    'floor/': lambda x,y: math.floor(x/y),
    'ceil/': lambda x,y: math.ceil(x/y),
    'round/': lambda x,y: round(x/y),

    '*': lambda x,y: x * y,
    'even': lambda x: x % 2 == 0,
    'odd': lambda x: x % 2 == 1,
    'min' : min,
    'max' : max,
    '==': lambda x,y: x == y,
    '≤': lambda x,y: x <= y,
    # '<=': lambda x,y: x <= y,
    '≥': lambda x,y: x >= y,
    # '>=': lambda x,y: x >= y,
    '<': lambda x,y: x < y,
    '>': lambda x,y: x > y,
    'not': lambda x: not x,
    'and': and_eval,
    'or': or_eval,
    '->': lambda x,y: (not x) or y,
    'days': lambda x: timedelta(days=x),
    # 'round': round,
    # 'ceil': math.ceil,

    # 'tuple' : lambda x,y: (x,y),
    'tuple' : lambda *x: tuple([*x]),
    'tupleGet' : lambda t,i: t[i],

    'mapSet' : tdmapSet,
    'tdmapAdd' : tdmapAdd,
    'delete' : deleteFromCollection,
    'hasKey' : colHas,
    'minValue' : tdmapMinValue,
    'tdmapHasItemExpiredBefore' : tdmapHasItemExpiredBefore,
    'tdGEQ' : tdmapTimeDeltaGEQ,
    'tdLEQ' : tdmapTimeDeltaLEQ,
    'tdLT' : tdmapTimeDeltaLT,
    'emptyTDMap' : lambda: tuple(),

    'nonempty' : lambda x: len(x) > 0,
    'empty' : lambda x: len(x) == 0,
    'size' : len,

    'emptySet': lambda: frozenset(),
    'add': lambda fset, x: fset.union({x}),
    'has': lambda fset, x: x in fset,
    'ifthenelse' : lambda t,tbranch,fbranch: tbranch if t else fbranch
}

def event_to_action_str(event:Event):
    if event.actionparam_subst:
        params_str = ", ".join([f"{key}: {event.actionparam_subst[key]}" for key in event.actionparam_subst.keys()])
        return f"{event.action_id}({params_str})"
    else:
        return f"{event.action_id}"

class EvalError(Exception):
    pass
