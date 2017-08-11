# from typing import Union, List, Set, Dict, Any, Tuple
import logging
from typing import List
from constants_and_defined_types import GlobalVarId
from parse_sexpr import SExpr

class GlobalVar:
    def __init__(self, name: GlobalVarId, sort: str, initval: str, modifier=None) -> None:
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
        self.varname : str = varname
        self.value_expr = value_expr

    # def varObj(self,env:'LSMTop'):
    #     if self.varname in top.global_vars:
    #         return top.global_vars[self.varname]
    #     else:
    #         logging.error("Todo: see if it's a local var")

    def __str__(self):
        return f"{self.varname} := {str(self.value_expr)}"

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

    def __str__(self):
        return "({} {})".format(self.head, " ".join([str(x) for x in self.args]))

class Atom(Term):
    def __init__(self, atom:str) -> None:
        self.atom = atom