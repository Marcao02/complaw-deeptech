import logging
from typing import List, Union, Optional, NamedTuple
from constants_and_defined_types import GlobalVarId
from parse_sexpr import SExpr, SExprOrStr

class GlobalVar(NamedTuple):
    name: GlobalVarId
    sort: str
    initval: str
    modifier:SExpr

    def __str__(self) -> str:
        return (str(self.modifier) + ' ' if self.modifier else '') + self.name + " : " + self.sort + " := " + self.initval

class Proposition:
    def __init__(self, lst:SExprOrStr) -> None:
        self.lst = lst

class ContractClaim(Proposition):
    # TODO
    pass

class CodeBlockStatement:
    """ Just the common parent """
    pass

class Term:
    pass

TermOrStr = Union[Term,str]

class FnApp(Term):
    def __init__(self, head:str, args:List[Term]) -> None:
        self.head = head
        self.args = args

    def __str__(self):
        return "({} {})".format(self.head, " ".join([str(x) for x in self.args]))

class Atom(Term):
    def __init__(self, atom:str) -> None:
        self.atom = atom

class VarAssignStatement(CodeBlockStatement):
    def __init__(self, varname:str, value_expr:TermOrStr, modifier:Optional[str] = None) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} := {str(self.value_expr)}"

class InCodeConjectureStatement(CodeBlockStatement):
    def __init__(self, exprs:List) -> None:
        self.value_exprs = exprs

class IncrementStatement(CodeBlockStatement):
    def __init__(self, varname:str, value_expr:TermOrStr) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} += {str(self.value_expr)}"

class DecrementStatement(CodeBlockStatement):
    def __init__(self, varname:str, value_expr:TermOrStr) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} -= {str(self.value_expr)}"

