from itertools import chain

from src.independent.typing_imports import *
from src.constants_and_defined_types import ContractParamId, SortId
from src.model.Term import Term
from src.model.Sort import Sort

T = TypeVar('T')

class ContractParamDec:
    def __init__(self, name: ContractParamId, sort: Sort, value_expr: Optional[Term]) -> None:
        self.name = name
        self.sort = sort
        self.value_expr = value_expr

    def forEach(self, pred: Callable[[Any], bool], f: Callable[[Any], Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        if pred(self.name):
            rviter = chain(rviter, f(self.name))
        if pred(self.sort):
            rviter = chain(rviter, f(self.sort))
        if self.value_expr and pred(self.value_expr):
            rviter = chain(rviter, f(self.value_expr), self.value_expr.forEach(pred,f))
        return rviter

    def __str__(self) -> str:
        if self.value_expr:
            return self.name + " : " + str(self.sort) + " := " + str(self.value_expr)
        else:
            return self.name + " : " + str(self.sort)
