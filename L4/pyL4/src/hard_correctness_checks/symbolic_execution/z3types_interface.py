from datetime import timedelta
from typing import NewType
from z3 import z3, z3num  # type:ignore
from z3.z3 import Solver # type:ignore

from src.independent.util import todo_once, chcaststr
from src.independent.typing_imports import *
from src.model.Sort import Sort as L4Sort


REALS_ONLY = True
OPTIONS = {
    # 'theory_aware_branching': True,
    # 'theory_case_split': True,
    # 'ite_extra': True,
    # 'ite_extra_rules': True,

    # 'print_stats': True,
    # 'push_ite_arith': True,
    'arith.ignore_int': REALS_ONLY, # hasn't made a difference...
    # 'timeout': 10, # yes, it works

}
class Z3Term:
    def decl(self,*args:Any) -> Any:
        pass
    def children(self,*args:Any) -> Any:
        pass
    def is_int(self) -> bool:
        pass
Z3TRUE = z3.BoolVal(True)
Z3FALSE = z3.BoolVal(False)

SORT_TO_SMTLIB_PRIM_TYPE : Dict[str, str] = {
    "$":"Real",
    "Pos$":"Real",
    "SharePrice":"Real",
    "Fraction[0,1)":"Real",
    "Fraction(0,1]":"Real",
    "Fraction[0,1]":"Real",
    "PosReal":"Real",
    "(0,1)":"Real",

    "TimeDelta":"Real" if REALS_ONLY else "Int",
    "PosTimeDelta":"Real" if REALS_ONLY else "Int",

    # "ShareCnt": "Int",
    # "PosShareCnt": "Int",
    # "Nat": "Int",
    # "Int": "Int",
    "ShareCnt": "Real" if REALS_ONLY else "Int",
    "PosShareCnt": "Real" if REALS_ONLY else "Int",
    "Nat": "Real" if REALS_ONLY else "Int",
    "Int": "Real" if REALS_ONLY else "Int",

    "Bool": "Bool",
}

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
def div(a1:Z3Term, a2:Z3Term) -> Z3Term:
    """
    Integer division when REALS_ONLY is false. Real division otherwise.
    """
    if REALS_ONLY:
        return a1 / a2 # type:ignore
    else:
        if a1.is_int():
            a1 = z3.ToReal(a1)
        if a2.is_int():
            a2 = z3.ToReal(a2)
        return z3.ToInt(a1 / a2) # type:ignore


# def fnapp(symb:str, *args:SMTExpr) -> SMTExprNonatom_:
#     return SMTExprNonatom(symb, args)

if REALS_ONLY:
    z30 = z3.RealVal(0)
    z31 = z3.RealVal(1)
else:
    z30 = z3.IntVal(0)
    z31 = z3.IntVal(1)
z3half = z3.RealVal(1/2)

z3interp : Dict[str,Any] = dict()
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

if REALS_ONLY:
    z3interp["ceil/"] = lambda a, b: ite(z3.IsInt(a / b), a / b, z3.ToReal(z3.ToInt(a / b)) + z31)  # type:ignore
    z3interp["floor/"] = lambda a,b: z3.ToReal(z3.ToInt(a / b))
else:
    # a round/ b is floor(a/b) + (1 if rem(a,b) >= floor(b/2) else 0   (and floor(a/b) is integer division)
    z3interp["ceil/"] = lambda a,b: ite(a / b == div(a,b),
                                        div(a,b),
                                        div(a,b) + z31) # type:ignore
    z3interp["floor/"] = div

z3interp["round/"] = lambda a,b: ite((a / b) - div(a, b) < z3half, # type:ignore
                                     z3interp["floor/"](a,b), # type:ignore
                                     z3interp["ceil/"](a,b))  # type:ignore


SORT_TO_PRED = cast(Dict[str,Callable[[Any], Z3Term]], {
    "TimeDelta": lambda x: x >= z30,
    "$": lambda x: x >= z30,
    "Pos$": lambda x: x > z30,
    "PosReal": lambda x: x > z30,
    "ShareCnt": lambda x: x >= z30,
    "Nat": lambda x: x >= z30,
    "(0,1)": lambda x: conj(x > z30, x < z31),
    "PosShareCnt": lambda x: x > z30,
    "SharePrice": lambda x: x >= z30,
    "Fraction[0,1)": lambda x: conj(x >= z30, x < z31),
    "Fraction[0,1]": lambda x: conj(x >= z30, x <= z31),
    "Fraction(0,1]": lambda x: conj(x > z30, x <= z31),
    "Fraction(0,1)": lambda x: conj(x > z30, x < z31)
})


def somewhatPrimValToZ3(val:Union[bool, int, float, timedelta], prog_timeunit:str) -> Z3Term:
    if isinstance(val,timedelta):
        return timedeltaToZ3(val,prog_timeunit)
    else:
        return primValToZ3(val)

def primValToZ3(val:Union[bool, int, float]) -> Z3Term:
    if isinstance(val, bool):
        return z3.BoolVal(val) # type:ignore
    elif isinstance(val, int):
        if REALS_ONLY:
            return z3.RealVal(val)
        else:
            return z3.IntVal(val) # type:ignore
    elif isinstance(val, float):
        return z3.RealVal(val) # type:ignore

    raise TypeError(str(val) + " of type " + str(type(val)))

def timedeltaToZ3(val:timedelta, prog_timeunit:str) -> Z3Term:
    if prog_timeunit == "m":
        num = ( val.total_seconds() / 60 ) # type:ignore
    elif prog_timeunit == "h":
        num = ( val.total_seconds() / (60*60) ) # type:ignore
    elif prog_timeunit == "d":
        num = ( val.days )  # type:ignore
    elif prog_timeunit == "s":
        num = ( val.total_seconds() )  # type:ignore
    else:
        raise ValueError
    if REALS_ONLY:
        return z3.RealVal(num) #type:ignore
    else:
        return z3.IntVal(num) #type:ignore


# todo_once("TEMP: this will silently fuck up if run code for two contracts in one thread")
# sort_accounted : set[str]

def name2symbolicvar(name:str,sort:L4Sort, sz3:Optional[Solver] = None) -> Z3Term:

    smtsort = SORT_TO_SMTLIB_PRIM_TYPE[chcaststr(sort)]

    rv : Z3Term
    if smtsort == "Int":
        if REALS_ONLY:
            rv = z3.Real(name)
        else:
            rv = z3.Int(name)  # type:ignore
    elif smtsort == "Real":
        rv = z3.Real(name)  # type:ignore
    elif smtsort == "Bool":
        rv = z3.Bool(name)  # type:ignore
    else:
        raise NotImplementedError(f"SMT sort {smtsort} unsupported")
    if sz3:
        if sort in SORT_TO_PRED:
            sz3.add(SORT_TO_PRED[cast(str,sort)](rv))
        else:
            print(f"Sort {sort} not in SORT_TO_PRED")
    return rv

def name2actparam_symbolic_var(name:str,t:int,sort:L4Sort, sz3:Optional[Solver] = None) -> Z3Term:
    return name2symbolicvar(name + "_" + str(t), sort,sz3)




def z3termPrettyPrint(_term:Z3Term) -> str:
    lst : List[str] = []

    def helper(term:Z3Term):
        if term.decl().name() == "and":
            for x in term.children():
                helper(x)
        elif str(term) != "True":
            lst.append(str(term))

    helper(_term)
    return f"And\n\t" + '\n\t'.join(lst)

def flatten(lst:list) -> list:
    rv : list = []
    def helper(y:Any):
        if isinstance(y, list) or isinstance(y, tuple):
            for x in y:
                helper(x)
        else:
            rv.append(y)
    helper(lst)
    return rv