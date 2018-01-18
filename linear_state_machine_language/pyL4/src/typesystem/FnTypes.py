from typing import Dict, Set

from src.typesystem.Sorts import *

SortTuple = Tuple[Sort,...]

class SimpleFnType(NamedTuple):
    parts: SortTuple
    @property
    def dom(self) -> SortTuple:
        return self.parts[:-1]
    @property
    def ran(self) -> Sort:
        return self.parts[0]

class ArbArityFnType(NamedTuple):
    dom: Sort
    ran: Sort

class OverloadedFnType(NamedTuple):
    parts: Tuple[Union[SimpleFnType,ArbArityFnType],...]
    range_memo: Dict[SortTuple, Sort]
    illtyped_memo: Set[Tuple[Sort, ...]]


