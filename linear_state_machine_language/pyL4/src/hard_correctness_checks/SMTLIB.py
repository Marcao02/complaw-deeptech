from typing import NewType

from src.independent.util_for_str import mapjoin
from src.constants_and_defined_types import ActionId
from src.model.Sort import Sort, SortOpApp
from src.independent.typing_imports import *

# Immutable recursive datatypes pattern
SMTAtom = Union[str, int, float, bool]
SMTExpr = Union['SMTExprNonatom_',SMTAtom]
class SMTExprNonatom_(NamedTuple):
    symb: str
    args_: Tuple[Any,...]
    @property # typed accessor
    def args(self) -> Tuple[SMTExpr,...]: return cast(Tuple[SMTExpr,...], self.args_)
    def __str__(self) -> str:
        return f"({self.symb} {' '.join((str(x) for x in self.args))})"
def SMTExprNonatom(symb:str, args:Iterable[SMTExpr]) -> SMTExprNonatom_: return SMTExprNonatom_(symb,tuple(args))

SMTCommand_ = NewType('SMTCommand_', SMTExprNonatom_)
def SMTCommand(symb:str, args:Iterable[SMTExpr]) -> SMTCommand_: return SMTExprNonatom(symb,args) # type:ignore

# def SMTCommandStar(symb:str, *args:SMTExpr) -> SMTCommand_: return SMTExprNonatom(symb,args) # type:ignore

SMTLine = Union[SMTCommand_, str]

SMT_BUILDIN_FNS = frozenset({'and', 'or', 'not', '=>', '=', '*', '+', '>', '<', '<=', '>=', '/', '-'})
INTERP_AS_2PROJECTION = frozenset({"cast", "check", "units", "trust"})

SORT_TO_SMTLIB_PRIM_TYPE : Dict[Sort, str] = {
    "$":"Real",
    "Pos$":"Real",
    "SharePrice":"Real",
    "Fraction[0,1)":"Real",
    "Fraction(0,1]":"Real",
    "Fraction[0,1]":"Real",
    "PosReal":"Real",
    "TimeDelta":"Real",

    "ShareCnt": "Int",
    "PosShareCnt": "Int",
    "Nat": "Int",

    "Bool": "Bool",

    SortOpApp("TDMap",(SortOpApp("Tuple",("Nat","Nat")),)): "TDMapNatNat",
    SortOpApp("Tuple",("Nat","Nat")) : "NatNat"

}

SORT_TO_PRED : Dict[str,Callable[[str], SMTExpr]]= {
    "TimeDelta": lambda x: fnapp(">", x, 0),
    "$": lambda x: fnapp(">=", x, 0),
    "Pos$": lambda x: fnapp(">", x, 0),
    "PosReal": lambda x: fnapp(">", x, 0),
    "ShareCnt": lambda x: fnapp(">=", x, 0),
    "Nat": lambda x: fnapp(">=", x, 0),
    "PosShareCnt": lambda x: fnapp(">", x, 0),
    "SharePrice": lambda x: fnapp(">=", x, 0),
    "Fraction[0,1)": lambda x: conj( fnapp(">=", x, 0), fnapp("<", x, 1) ),
    "Fraction[0,1]": lambda x: conj( fnapp(">=", x, 0), fnapp("<=", x, 1) ),
    "Fraction(0,1]": lambda x: conj( fnapp(">", x, 0), fnapp("<=", x, 1) )
}

ENV_VAR_SUBST : Dict[str,str] = {
    "event_td":"event_td",
    "event_role":"event_role"
}

FN_NAME_SUBST : Dict[str,str] = {
    'ifthenelse': 'ite',
    '≤' : '<=',
    '≥' : '>=',
    '==' : '=',

    # hacky and temporary
    'mapSet': 'mapSet',
    'mapDelete': 'mapDelete',
    'tuple': 'tuple',

}


# We could alternatively use SMTLIB `define-fun` command for these, but I believe the result would be the same.
MACRO_DEFINED_FNS : Dict[str, Callable] = {
    "days": lambda x: x,
    "cast": lambda S,t: t,
    "check": lambda S,t: t,
    "trust": lambda S,t: t,
    'fraction-of-sum': lambda a,b: fnapp('/', a, fnapp('+', a, b)),
    'min': lambda a,b: ite(fnapp('<', a, b), a, b),
    'even': lambda a: equals(fnapp('mod', a, 2), 0),
    'odd': lambda a: equals(fnapp('mod', a, 2), 1),
    'max': lambda a,b: ite(fnapp('>', b, a), b, a),
     # a round/ b is floor(a/b) + (1 if rem(a,b) >= floor(b/2) else 0   (and floor(a/b) is integer division)
    'round/': lambda a,b: fnapp('+', fnapp('div', a, b), ite( fnapp('>=', fnapp('rem',a,b), fnapp('div',b,2)), 1, 0)),
    # a floor/ b is a integer/ b
    'floor/': lambda a,b: fnapp('div', a, b),
    # a ceil/ b is floor(a/b) + (1 if floor(a/b) < a/b else 0)
    'ceil/': lambda a,b: fnapp('+', fnapp('div', a, b), ite( fnapp('<', fnapp('div',a,b), fnapp('/',a,b)), 1, 0)),

}


def disj(*args:SMTExpr) -> SMTExprNonatom_:
    return SMTExprNonatom("or", args)
def conj(*args:SMTExpr) -> SMTExprNonatom_:
    return SMTExprNonatom("and", args)
def neg(arg:SMTExpr) -> SMTExprNonatom_:
    return SMTExprNonatom("not", (arg,))
def implies(arg1:SMTExpr, arg2:SMTExpr) -> SMTExprNonatom_:
    return SMTExprNonatom("=>", (arg1,arg2))
def equals(arg1:SMTExpr, arg2:SMTExpr) -> SMTExprNonatom_:
    return SMTExprNonatom("=", (arg1, arg2))
def ite(a1:SMTExpr, a2:SMTExpr, a3:SMTExpr) -> SMTExprNonatom_:
    return SMTExprNonatom('ite', (a1, a2, a3))

def disjoint(*args:SMTExpr) -> SMTExprNonatom_:
    parts : List[SMTExprNonatom_] = []
    for a in args:
        for b in args:
            if a != b:
                parts.append(implies( a, neg(b) ))
    return conj(*parts)

def disjoint_exhaustive(*args:SMTExpr) -> SMTExprNonatom_:
    return conj(disjoint(*args), disj(*args))

def fnapp(symb:str, *args:SMTExpr) -> SMTExprNonatom_:
    return SMTExprNonatom(symb, args)

def declareconst(const:str, sort:str) -> SMTCommand_:
    return SMTCommand('declare-const', (const, sort))
def assertexpr(expr:SMTExpr) -> SMTCommand_:
    return SMTCommand('assert' , (expr,))


def to_smt_primed_token(s:str) -> str:
    return s + "_next"
def to_smt_unprimed_token(s:str) -> str:
    assert s.endswith("_next")
    return s[-5:]
def is_smt_primed_token(s:str):
    return s.endswith("_next")

def rename_with_action_scope(actionparam_name:str, actionid:ActionId) -> str:
    return actionparam_name + "_" + actionid

def smtlib_expr_to_str(x:Union[SMTExpr, str, int, float, bool]) -> str:
    if isinstance(x,(str,int,float,bool)):
        return str(x)
    # else:
    #     return f"({mapjoin(smtlib_expr_to_str, x, ' ')})"
    else:
        # print(x)
        # rv = f"({x.symb} "
        # for arg in x.args:
        #     print(arg)
        # rv += ")"
        # return rv
        return f"({x.symb} {mapjoin(smtlib_expr_to_str, x.args, ' ')})"

SMTLIB_OUTPUT_INDENT_SIZE = 2
def smt_lines_to_str(lines:List[SMTLine]) -> str:
    rv = ""
    indent = 0
    for line in lines:
        if line == "(pop)":
            indent -= 1

        if isinstance(line,str):
            rv += f"{indent*SMTLIB_OUTPUT_INDENT_SIZE*' '}{line}\n"
        else:
            # rv += f"{indent*SMTLIB_OUTPUT_INDENT_SIZE *' '}({mapjoin(smtlib_expr_to_str, line, ' ')})\n"
            rv += f"{indent*SMTLIB_OUTPUT_INDENT_SIZE *' '}{smtlib_expr_to_str(line)}\n"

        if line == "(push)":
            indent += 1
    return rv


def sort2smtlibprimtype(s: Sort) -> str:
    if s in SORT_TO_SMTLIB_PRIM_TYPE:
        return SORT_TO_SMTLIB_PRIM_TYPE[s]
    print(f"sort {s} of type {type(s)} not in SORT_TO_SMTLIB_PRIM_TYPE")

    raise NotImplementedError()
