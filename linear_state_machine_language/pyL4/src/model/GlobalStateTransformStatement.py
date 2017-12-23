
from typing import List, Union, Optional, NamedTuple

from src.model.constants_and_defined_types import GlobalVarId, StateTransformLocalVarId
from src.model.Term import Term

from src.model.util import castid, indent


class GlobalStateTransformStatement:
    """ Just the common parent """
    def toStr(self, i:int):
        return indent(i) + str(self)


class IfElse(GlobalStateTransformStatement):
    def __init__(self, test:Term,
                 true_branch:List[GlobalStateTransformStatement],
                 false_branch: List[GlobalStateTransformStatement]) -> None:
        self.test = test
        self.true_branch = true_branch
        self.false_branch = false_branch

    def toStr(self,i:int):
        rv = indent(i) + f"if {self.test}:\n"
        for x in self.true_branch:
            rv += indent(i + 1) + str(x) + "\n"
        rv += indent(i) + "else:\n"
        for x in self.false_branch:
            rv += indent(i + 1) + str(x) + "\n"
        return rv


class GlobalVarAssignStatement(GlobalStateTransformStatement):
    def __init__(self, varname:GlobalVarId, value_expr:Term) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} := {str(self.value_expr)}"

class StateTransformLocalVarDec(GlobalStateTransformStatement):
    def __init__(self, varname:StateTransformLocalVarId, value_expr:Term, sort:str) -> None:
        self.varname = varname
        self.value_expr = value_expr
        self.sort = sort

    def __str__(self):
        return f"{self.varname} : {self.sort} := {str(self.value_expr)}"

# class LocalVarDec(GlobalVarAssignStatement):
#     def __init__(self, varname:LocalVarId, value_expr:Term, sort:str) -> None:
#         super().__init__(castid(GlobalVarId,varname), value_expr)
#         self.sort = sort
#
#     def __str__(self):
#         return f"{self.varname} := {str(self.value_expr)}"

class InCodeConjectureStatement(GlobalStateTransformStatement):
    def __init__(self, prop:Term) -> None:
        self.value_expr = prop

    def __str__(self) -> str:
        return "prove " + str(self.value_expr)

class IncrementStatement(GlobalStateTransformStatement):
    def __init__(self, varname:GlobalVarId, value_expr:Term) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} += {str(self.value_expr)}"
class DecrementStatement(GlobalStateTransformStatement):
    def __init__(self, varname:GlobalVarId, value_expr:Term) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} -= {str(self.value_expr)}"
class TimesEqualsStatement(GlobalStateTransformStatement):
    def __init__(self, varname:GlobalVarId, value_expr:Term) -> None:
        self.varname = varname
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.varname} *= {str(self.value_expr)}"
