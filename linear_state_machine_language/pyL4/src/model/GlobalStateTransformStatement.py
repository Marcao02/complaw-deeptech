
from typing import List, Optional, cast

from src.model.GlobalVarDec import GlobalVarDec
from src.constants_and_defined_types import GlobalVarId, StateTransformLocalVarId
from src.model.Term import Term
from src.util import indent

""" Just the common parent """
class GlobalStateTransformStatement:
    def __init__(self):
        self.orig : Optional[GlobalStateTransformStatement]

    def toStr(self, i:int):
        return indent(i) + str(self)


class IfElse(GlobalStateTransformStatement):
    def __init__(self, test:Term,
                 true_branch:List[GlobalStateTransformStatement],
                 false_branch: Optional[List[GlobalStateTransformStatement]] = None) -> None:
        super().__init__()
        self.test = test
        self.true_branch = true_branch
        self.false_branch = false_branch

    def toStr(self,i:int):
        rv = indent(i) + f"if {self.test}:\n"
        for x in self.true_branch:
            rv += indent(i + 1) + str(x) + "\n"
        if self.false_branch:
            rv += indent(i) + "else:\n"
            for x in self.false_branch:
                rv += indent(i + 1) + str(x) + "\n"
        return rv


class StateTransformLocalVarDec(GlobalStateTransformStatement):
    def __init__(self, varname:StateTransformLocalVarId, value_expr:Term, sort:str) -> None:
        super().__init__()
        self.varname : StateTransformLocalVarId = varname
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
        super().__init__()
        self.value_expr = prop

    def __str__(self) -> str:
        return "prove " + str(self.value_expr)


class AbstractGlobalVarAssignStatement(GlobalStateTransformStatement):
    def __init__(self, vardec:GlobalVarDec, value_expr:Term) -> None:
        super().__init__()
        self.vardec = vardec
        self.value_expr = value_expr

    @property
    def varname(self) -> GlobalVarId:
        # this cast shouldn't be necessary. weird.
        # return cast(GlobalVarId,self.vardec.name)
        return self.vardec.name

class GlobalVarAssignStatement(AbstractGlobalVarAssignStatement):
    def __str__(self):
        return f"{self.varname} := {str(self.value_expr)}"
class IncrementStatement(AbstractGlobalVarAssignStatement):
    def __str__(self):
        return f"{self.varname} += {str(self.value_expr)}"
class DecrementStatement(AbstractGlobalVarAssignStatement):
    def __str__(self):
        return f"{self.varname} -= {str(self.value_expr)}"
class TimesEqualsStatement(AbstractGlobalVarAssignStatement):
    def __str__(self):
        return f"{self.varname} *= {str(self.value_expr)}"
