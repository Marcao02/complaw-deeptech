from itertools import chain

from src.independent.FileCoord import FileCoord
from src.independent.util import castid, todo_once
from src.constants_and_defined_types import StateVarId, ContractParamId, ActionBoundActionParamId, \
    RuleBoundActionParamId, LocalVarId
from src.independent.typing_imports import *
from src.model.Action import Action
from src.model.ActionRule import ActionRule
from src.model.ContractParamDec import ContractParamDec
from src.model.Statement import LocalVarDec
from src.model.StateVarDec import StateVarDec
from src.model.Term import Term


def primed(s:StateVarId) -> StateVarId:
    return s + "'" # type:ignore


T = TypeVar('T')

class BoundVar(Term):
    def __init__(self, coord: Optional[FileCoord] = None) -> None:
        super().__init__(coord)
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

    def substForVar(self, var: str, term: 'Term') -> 'Term':
        return term if self.name == var else self

    def __eq__(self, other:Any) -> bool:
        todo_once("BoundVar.__eq__ would be unsafe if variable shadowing was allowed, which it current isn't.")
        return isinstance(other,BoundVar) and type(self) == type(other) and self.name == other.name

    def __str__(self):
        return self.name
    def __repr__(self):
        return self.name

    @property
    def name(self) -> str:
        raise NotImplementedError

class RuleBoundActionParam(BoundVar):
    def __init__(self, _name:RuleBoundActionParamId, conn: ActionRule, ind:int, coord: Optional[FileCoord] = None) -> None:
        super().__init__(coord)
        self.action_rule = conn
        self._name = _name
        self.ind = ind

    @property
    def name(self) -> RuleBoundActionParamId:
        return self._name


class ActionBoundActionParam(BoundVar):
    def __init__(self, _name:ActionBoundActionParamId, action: Action, ind:int, coord: Optional[FileCoord] = None) -> None:
        super().__init__(coord)
        self.action = action
        self._name = _name
        self.ind = ind

    @property
    def name(self) -> ActionBoundActionParamId:
        return self._name


class LocalVar(BoundVar):
    def __init__(self, vardec: LocalVarDec, coord: Optional[FileCoord] = None) -> None:
        super().__init__(coord)
        self.vardec : LocalVarDec = vardec

    @property
    def name(self) -> LocalVarId:
        # return cast(LocalVarId, self.vardec.varname)
        return self.vardec.varname


class StateVar(BoundVar):
    def __init__(self, vardec:StateVarDec, coord: Optional[FileCoord] = None) -> None:
        super().__init__(coord)
        self.vardec : StateVarDec = vardec

    @property
    def name(self) -> StateVarId:
        return self.vardec.name

class PrimedStateVar(BoundVar):
    def __init__(self, vardec:StateVarDec, coord: Optional[FileCoord] = None) -> None:
        super().__init__(coord)
        self.vardec : StateVarDec = vardec

    @property
    def name(self) -> StateVarId:
        return primed(self.vardec.name)

class ContractParam(BoundVar):
    def __init__(self, paramdec:ContractParamDec, coord: Optional[FileCoord] = None) -> None:
        super().__init__(coord)
        self.paramdec : ContractParamDec = paramdec

    @property
    def name(self) -> ContractParamId:
        # cast shouldn't be necessary
        return castid(ContractParamId,self.paramdec.name)

    # what?

