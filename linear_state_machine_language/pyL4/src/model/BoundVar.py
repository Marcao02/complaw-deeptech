from itertools import chain

from src.independent.util import castid
from src.constants_and_defined_types import StateVarId, ContractParamId, ActionBoundActionParamId, \
    RuleBoundActionParamId, LocalVarId
from src.independent.typing_imports import *
from src.model.Action import Action
from src.model.ActionRule import ActionRule
from src.model.ContractParamDec import ContractParamDec
from src.model.Statement import LocalVarDec
from src.model.StateVarDec import StateVarDec
from src.model.Term import Term


class BoundVar(Term):
    def __init__(self) -> None:
        super().__init__()
        pass

    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe: Optional[Iterable[T]] = None) -> Iterable[T]:
        if iteraccum_maybe:
            return chain(iteraccum_maybe,f(self))
        else:
            return f(self)

    def forEach(self, pred: Callable[[Any], bool], f: Callable[[Any], Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        if pred(self.name):
            rviter = chain(rviter, f(self.name))
        return rviter


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


class LocalVar(BoundVar):
    def __init__(self, vardec: LocalVarDec) -> None:
        super().__init__()
        self.vardec : LocalVarDec = vardec

    @property
    def name(self) -> LocalVarId:
        # return cast(LocalVarId, self.vardec.varname)
        return self.vardec.varname


class GlobalVar(BoundVar):
    def __init__(self, vardec:StateVarDec) -> None:
        super().__init__()
        self.vardec : StateVarDec = vardec

    @property
    def name(self) -> StateVarId:
        return self.vardec.name


class ContractParam(BoundVar):
    def __init__(self, paramdec:ContractParamDec) -> None:
        super().__init__()
        self.paramdec : ContractParamDec = paramdec

    @property
    def name(self) -> ContractParamId:
        # cast shouldn't be necessary
        return castid(ContractParamId,self.paramdec.name)

    # what?

