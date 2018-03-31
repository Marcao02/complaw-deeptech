from datetime import timedelta
from typing import NewType
from z3 import z3, z3num, Solver  # type:ignore

from src.independent.util import todo_once
from src.hard_correctness_checks.SMTLIB import SORT_TO_SMTLIB_PRIM_TYPE
from src.independent.typing_imports import *
from src.model.Sort import Sort as L4Sort

Z3Term = NewType('Z3Term',object)
Z3TRUE = z3.BoolVal(True)
Z3FALSE = z3.BoolVal(False)

def conj(*forms:Z3Term) -> Z3Term:
    return z3.And(*forms) # type:ignore
def neg(form:Z3Term) -> Z3Term:
    return z3.Not(form) # type:ignore
def disj(*forms:Z3Term) -> Z3Term:
    return z3.Or(forms) # type:ignore
def implies(arg1:Z3Term, arg2:Z3Term) -> Z3Term:
    return z3.Implies(arg1,arg2)   # type:ignore
def equals(arg1:Z3Term, arg2:Z3Term) -> Z3Term:
    return arg1 == arg2 # type:ignore
def ite(a1:Z3Term, a2:Z3Term, a3:Z3Term) -> Z3Term:
    return z3.If(a1, a2, a3) # type:ignore
def div(a1:Z3Term,a2:Z3Term) -> Z3Term:
    return z3.ToInt(a1 / a2)


# def fnapp(symb:str, *args:SMTExpr) -> SMTExprNonatom_:
#     return SMTExprNonatom(symb, args)


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
z3interp["not"] = neg
z3interp["and"] = conj
z3interp["or"] = disj
z3interp["even"] = lambda x: (x % 2) == 0
z3interp["min"] = lambda x,y: ite(x < y, x, y)

# a round/ b is floor(a/b) + (1 if rem(a,b) >= floor(b/2) else 0   (and floor(a/b) is integer division)
z3interp["ceil/"] = lambda a,b: ite(z3.IsInt(a / b), div(a,b), div(a,b) + 1)
z3interp["floor/"] = div
z3interp["round/"] = lambda a,b: ite((a / b) - div(a,b) < 1/2, div(a,b), div(a,b) + 1)


SORT_TO_PRED = cast(Dict[str,Callable[[any], Z3Term]], {
    "TimeDelta": lambda x: x >= 0,
    "$": lambda x: x >= 0,
    "Pos$": lambda x: x > 0,
    "PosReal": lambda x: x > 0,
    "ShareCnt": lambda x: x >= 0,
    "Nat": lambda x: x >= 0,
    "PosShareCnt": lambda x: x > 0,
    "SharePrice": lambda x: x >= 0,
    "Fraction[0,1)": lambda x: conj(x >= 0, x < 1),
    "Fraction[0,1]": lambda x: conj(x >= 0, x <= 1),
    "Fraction(0,1]": lambda x: conj(x > 0, x <= 1),
    "Fraction(0,1)": lambda x: conj(x > 0, x < 1)
})

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