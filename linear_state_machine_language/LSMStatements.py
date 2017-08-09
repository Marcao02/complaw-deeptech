# from typing import Union, List, Set, Dict, Any, Tuple
from typing import List
from constants_and_defined_types import SExpr

class GlobalVar:
    def __init__(self, name: str, sort: str, initval: str, modifier=None) -> None:
        self.name = name
        self.sort = sort
        self.initval = initval
        self.modifier = modifier

    def __str__(self) -> str:
        if self.modifier:
            return self.modifier + ' ' + self.name + " : " + self.sort + " := " + self.initval
        else:
            return self.name + " : " + self.sort + " := " + self.initval

    def __repr__(self) -> str:
        if self.modifier:
            return "GlobalVar(" + self.name + ", " + self.sort + ", " + self.initval + ", " + self.modifier + ")"
        else:
            return "GlobalVar(" + self.name + ", " + self.sort + ", " + self.initval + ")"


class Proposition:
    """docstring for Proposition"""
    def __init__(self, lst) -> None:
        self.lst = lst

class ContractClaim(Proposition):
    """docstring for ContractClaim"""
    # TODO
    pass

class CodeBlockStatement:
    """ Just the common parent """
    pass

class VarAssignStatement(CodeBlockStatement):
    def __init__(self, varname:str, value_expr, modifier=None) -> None:
        self.var = varname
        self.value_expr = value_expr

class InCodeConjectureStatement(CodeBlockStatement):
    def __init__(self, exprs:List) -> None:
        self.value_exprs = exprs

class IncrementStatement(CodeBlockStatement):
    def __init__(self, varname:str, value_expr) -> None:
        self.var = varname
        self.value_expr = value_expr

class DecrementStatement(CodeBlockStatement):
    def __init__(self, varname:str, value_expr) -> None:
        self.var = varname
        self.value_expr = value_expr

class Term:
    pass

class FnApp(Term):
    def __init__(self, head:str, args:SExpr) -> None:
        self.head = head
        self.args = args

class Atom(Term):
    def __init__(self, atom:str) -> None:
        self.atom = atom