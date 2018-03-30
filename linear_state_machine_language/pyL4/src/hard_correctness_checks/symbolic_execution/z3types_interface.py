from datetime import timedelta
from typing import NewType
from z3 import z3, z3num # type:ignore

from src.hard_correctness_checks.SMTLIB import SORT_TO_SMTLIB_PRIM_TYPE
from src.independent.typing_imports import *
from src.model.Sort import Sort as L4Sort

Z3Term = NewType('Z3Term',object)
Z3TRUE = True
Z3FALSE = False

def z3and(*forms:Z3Term) -> Z3Term:
    return z3.And(*forms) # type:ignore
def z3not(form:Z3Term) -> Z3Term:
    return z3.Not(form) # type:ignore

z3interp : Any = dict()
z3interp["<"] = lambda x,y: x < y
z3interp[">"] = lambda x,y: x > y
z3interp["≥"] = lambda x,y: x >= y
z3interp["≤"] = lambda x,y: x <= y
z3interp["+"] = lambda x,y: x + y
z3interp["-"] = lambda x,y: x - y
z3interp["*"] = lambda x,y: x * y
z3interp["=="] = lambda x,y: x == y
z3interp["not"] = lambda x: z3not(x)
z3interp["and"] = lambda *x: z3and(x)
z3interp["even"] = lambda x: (x % 2) == 0

def primValToZ3(val:Union[bool, int, float]) -> Z3Term:
    if isinstance(val, bool):
        return z3.BoolVal(val) # type:ignore
    elif isinstance(val, int):
        return z3.IntVal(val) # type:ignore
    elif isinstance(val, float):
        return z3.RealVal(val) # type:ignore
    raise TypeError(str(val) + " of type " + str(type(val)))

def timedeltaToZ3(val:timedelta, prog_timeunit:str) -> Z3Term:
    if prog_timeunit == "m":
        return z3.RealVal( val.seconds / 60 ) # type:ignore
    elif prog_timeunit == "h":
        return z3.RealVal( val.seconds / (60*60) )
    elif prog_timeunit == "s":
        return z3.RealVal( val.seconds )
    else:
        raise ValueError



def name2symbolicvar(name:str,sort:L4Sort) -> Z3Term:
    print(f"name2symbolicvar({name},{sort})")
    smtsort = SORT_TO_SMTLIB_PRIM_TYPE[sort]
    if smtsort == "Int":
        return z3.Int(name)  # type:ignore
    elif smtsort == "Real":
        return z3.Real(name)  # type:ignore
    elif smtsort == "Bool":
        return z3.Bool(name)  # type:ignore
    raise NotImplementedError(f"SMT sort {smtsort} unsupported")
