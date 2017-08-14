import logging
from typing import List, Union, Optional, NamedTuple
from constants_and_defined_types import GlobalVarId
from parse_sexpr import SExpr, SExprOrStr


class Term:
    pass

class GlobalVarDec(NamedTuple):
    name: GlobalVarId
    sort: str
    initval: Optional[Term]
    modifier:SExpr

    def __str__(self) -> str:
        return (' '.join(self.modifier) + ' ' if self.modifier else '') + \
               self.name + " : " + self.sort + \
               (" := " + str(self.initval) if self.initval else '')

class Proposition:
    def __init__(self, lst:SExprOrStr) -> None:
        self.lst = lst

class ContractClaim(Proposition):
    # TODO
    pass

class CodeBlockStatement:
    """ Just the common parent """
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

    def __str__(self):
        return "@" + self.atom


class LocalVar(Term):
    def __init__(self, name:str) -> None:
        self.name = name

class GlobalVar(Term):
    def __init__(self, name:str, vardec:GlobalVarDec) -> None:
        self.name = name
        self.vardec = vardec

class Literal(Term):
    def __repr__(self):
        return str(self)

class IntLit(Literal):
    def __init__(self, lit:int) -> None:
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class BoolLit(Literal):
    def __init__(self, lit:bool) -> None:
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class DeadlineLit(Literal):
    def __init__(self, lit:str) -> None:
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class StringLit(Literal):
    def __init__(self, lit:str) -> None:
        self.lit = lit
    def __str__(self):
        return "'" + self.lit + "'"

class ContractParamDec(NamedTuple):
    name: str
    sort: str
    value_expr: Term

    def __str__(self) -> str:
        return self.name + " : " + self.sort + " := " + str(self.value_expr)

class VarAssignStatement(CodeBlockStatement):
    def __init__(self, varname:str, value_expr:Term) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} := {str(self.value_expr)}"

class LocalVarDec(VarAssignStatement):
    def __init__(self, varname:str, value_expr:Term, sort:str) -> None:
        super().__init__(varname, value_expr)
        self.sort = sort

    def __str__(self):
        return f"{self.varname} := {str(self.value_expr)}"

class InCodeConjectureStatement(CodeBlockStatement):
    def __init__(self, exprs:List) -> None:
        self.value_exprs = exprs

class IncrementStatement(CodeBlockStatement):
    def __init__(self, varname:str, value_expr:Term) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} += {str(self.value_expr)}"

class DecrementStatement(CodeBlockStatement):
    def __init__(self, varname:str, value_expr:Term) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} -= {str(self.value_expr)}"

