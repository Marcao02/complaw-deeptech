from typing import cast

from model.Action import Action
from model.ActionRule import NextActionRule, ActionRule
from model.GlobalVarDec import GlobalVarDec
from model.ContractParamDec import ContractParamDec
from model.GlobalStateTransformStatement import LocalVarDec
from model.Term import Term
from model.constants_and_defined_types import LocalVarId, GlobalVarId, ContractParamId, ActionParamId_BoundBy_ActionDecl, \
    ActionParamId_BoundBy_ActionRule


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

class ActionRuleDeclActionParam(BoundVar):
    def __init__(self, _name:ActionParamId_BoundBy_ActionRule, conn: ActionRule) -> None:
        super().__init__()
        self.action_rule = conn
        self._name = _name

    @property
    def name(self) -> ActionParamId_BoundBy_ActionRule:
        return self._name


class ActionDeclActionParam(BoundVar):
    def __init__(self, _name:ActionParamId_BoundBy_ActionDecl, action: Action) -> None:
        super().__init__()
        self.action = action
        self._name = _name

    @property
    def name(self) -> ActionParamId_BoundBy_ActionDecl:
        return self._name


class LocalVar(BoundVar):
    def __init__(self, vardec: LocalVarDec) -> None:
        super().__init__()
        self.vardec : LocalVarDec = vardec

    @property
    def name(self) -> LocalVarId:
        return cast(LocalVarId,self.vardec.varname)


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