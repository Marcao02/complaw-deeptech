from datetime import timedelta
from typing import NewType
from z3 import z3, z3num, Solver  # type:ignore

from src.independent.util import todo_once
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
z3interp["/"] = lambda x,y: x / y
z3interp["=="] = lambda x,y: x == y
z3interp["not"] = lambda x: z3not(x)
z3interp["and"] = lambda *x: z3and(x)
z3interp["even"] = lambda x: (x % 2) == 0
z3interp["min"] = lambda x,y: z3.If(x < y, x, y)

todo_once("INTERP OF ROUND/ IS A LIE!")
z3interp["round/"] = lambda x,y: x / y
z3interp["floor/"] = lambda x,y: x / y

SORT_TO_PRED : Dict[str,Callable[[any], Z3Term]]= {
    "TimeDelta": lambda x: x >= 0,
    "$": lambda x: x >= 0,
    "Pos$": lambda x: x > 0,
    "PosReal": lambda x: x > 0,
    "ShareCnt": lambda x: x >= 0,
    "Nat": lambda x: x >= 0,
    "PosShareCnt": lambda x: x > 0,
    "SharePrice": lambda x: x >= 0,
    "Fraction[0,1)": lambda x: z3and( x >= 0, x < 1 ),
    "Fraction[0,1]": lambda x: z3and( x >= 0, x <= 1 ),
    "Fraction(0,1]": lambda x: z3and( x > 0, x <= 1 ),
    "Fraction(0,1)": lambda x: z3and( x > 0, x < 1 )
}

todo_once("replace unicode symbols with ascii in AST")

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
        return z3.RealVal( val.total_seconds() / 60 ) # type:ignore
    elif prog_timeunit == "h":
        return z3.RealVal( val.total_seconds() / (60*60) )
    elif prog_timeunit == "d":
        return z3.RealVal( val.days )
    elif prog_timeunit == "s":
        return z3.RealVal( val.total_seconds() )
    else:
        raise ValueError



def name2symbolicvar(name:str,sort:L4Sort, sz3:Optional[Solver] = None) -> Z3Term:
    smtsort = SORT_TO_SMTLIB_PRIM_TYPE[sort]
    if smtsort == "Int":
        rv = z3.Int(name)  # type:ignore
    elif smtsort == "Real":
        rv = z3.Real(name)  # type:ignore
    elif smtsort == "Bool":
        rv = z3.Bool(name)  # type:ignore
    else:
        raise NotImplementedError(f"SMT sort {smtsort} unsupported")
    if sz3:
        if sort in SORT_TO_PRED:
            sz3.add(SORT_TO_PRED[sort](rv))
        else:
            print(f"Sort {sort} not in SORT_TO_PRED")
    return rv

def name2actparam_symbolic_var(name:str,t:int,sort:L4Sort, sz3:Optional[Solver] = None) -> Z3Term:
    return name2symbolicvar(name + "_" + str(t), sort,sz3)

def name2initstateval_symbolic_var(name:str,sort:L4Sort, sz3:Optional[Solver] = None) -> Z3Term:
    return name2symbolicvar(name,sort,sz3)



def z3termPrettyPrint(_term:Z3Term) -> str:
    lst = []

    def helper(term:Z3Term):
        if term.decl().name() == "and":
            for x in term.children():
                helper(x)
        elif str(term) != "True":
            lst.append(str(term))

    helper(_term)
    return f"And\n\t" + '\n\t'.join(lst)

def flatten(lst:list):
    rv = []
    def helper(y:any):
        if isinstance(y, list) or isinstance(y, tuple):
            for x in y:
                helper(x)
        else:
            rv.append(y)
    return rv