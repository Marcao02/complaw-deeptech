from typing import cast

from src.model.Action import Action
from src.model.ActionRule import NextActionRule, ActionRule
from src.model.GlobalVarDec import GlobalVarDec
from src.model.ContractParamDec import ContractParamDec
from src.model.Term import Term
from src.model.constants_and_defined_types import GlobalVarId, ContractParamId, ActionBoundActionParamId, \
    RuleBoundActionParamId


class BoundVar(Term):
    def __init__(self) -> None:
        super().__init__()
        pass

    def __str__(self):
        return self.name
    def __repr__(self):
        return self.name

    @property
    def name(self) -> str:
        raise NotImplementedError

class RuleBoundActionParam(BoundVar):
    def __init__(self, _name:RuleBoundActionParamId, conn: ActionRule, ind:int) -> None:
        super().__init__()
        self.action_rule = conn
        self._name = _name
        self.ind = ind

    @property
    def name(self) -> RuleBoundActionParamId:
        return self._name


class ActionBoundActionParam(BoundVar):
    def __init__(self, _name:ActionBoundActionParamId, action: Action, ind:int) -> None:
        super().__init__()
        self.action = action
        self._name = _name
        self.ind = ind

    @property
    def name(self) -> ActionBoundActionParamId:
        return self._name


# class LocalVar(BoundVar):
#     def __init__(self, vardec: LocalVarDec) -> None:
#         super().__init__()
#         self.vardec : LocalVarDec = vardec
#
#     @property
#     def name(self) -> LocalVarId:
#         return cast(LocalVarId,self.vardec.varname)


class GlobalVar(BoundVar):
    def __init__(self, vardec:GlobalVarDec) -> None:
        super().__init__()
        self.vardec : GlobalVarDec = vardec

    @property
    def name(self) -> GlobalVarId:
        return self.vardec.name


class ContractParam(BoundVar):
    def __init__(self, paramdec:ContractParamDec) -> None:
        super().__init__()
        self.paramdec : ContractParamDec = paramdec

    @property
    def name(self) -> ContractParamId:
        return self.paramdec.name