
from typing import List, Union, Optional, NamedTuple

from model.Term import Term
from model.constants_and_defined_types import LocalVarId, LocalOrGlobalVarId
from model.util import castid


class GlobalStateTransformStatement:
    """ Just the common parent """
    pass

class VarAssignStatement(GlobalStateTransformStatement):
    def __init__(self, varname:LocalOrGlobalVarId, value_expr:Term) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} := {str(self.value_expr)}"

class LocalVarDec(VarAssignStatement):
    def __init__(self, varname:LocalVarId, value_expr:Term, sort:str) -> None:
        super().__init__(castid(LocalOrGlobalVarId,varname), value_expr)
        self.sort = sort

    def __str__(self):
        return f"{self.varname} := {str(self.value_expr)}"

class InCodeConjectureStatement(GlobalStateTransformStatement):
    def __init__(self, prop:Term) -> None:
        self.value_expr = prop

    def __str__(self) -> str:
        return "prove " + str(self.value_expr)

class IncrementStatement(GlobalStateTransformStatement):
    def __init__(self, varname:LocalOrGlobalVarId, value_expr:Term) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} += {str(self.value_expr)}"
class DecrementStatement(GlobalStateTransformStatement):
    def __init__(self, varname:LocalOrGlobalVarId, value_expr:Term) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} -= {str(self.value_expr)}"
class TimesEqualsStatement(GlobalStateTransformStatement):
    def __init__(self, varname:LocalOrGlobalVarId, value_expr:Term) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} *= {str(self.value_expr)}"
