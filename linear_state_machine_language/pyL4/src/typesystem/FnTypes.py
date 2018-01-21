from typing import Dict, Set, List

from src.typesystem.Sorts import *
from src.util import mapjoin

SortTuple = Tuple[Sort,...]

class SimpleFnType(NamedTuple):
    parts: SortTuple
    @property
    def dom(self) -> SortTuple:
        return self.parts[:-1]
    @property
    def ran(self) -> Sort:
        return self.parts[-1]

    def __str__(self) -> str:
        todo_once("contrib: why cast necessary?")
        return mapjoin(str,self.parts,' -> ')
    def __repr__(self) -> str:
        return str(self)

class ArbArityFnType(NamedTuple):
    dom: Sort
    ran: Sort
    def __str__(self) -> str:
        return f"{self.dom}* -> {self.ran}"
    def __repr__(self) -> str:
        return str(self)

NonoverloadedFnType = Union[SimpleFnType,ArbArityFnType]


class OverloadedFnType(NamedTuple):
    parts: List[NonoverloadedFnType]
    range_memo: Dict[SortTuple, Sort]
    illtyped_memo: Set[Tuple[Sort, ...]]

    def __str__(self) -> str:
        return mapjoin(str, self.parts, "\n")
