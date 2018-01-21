from typing import NamedTuple, Tuple, Any, Union, cast, NewType, Optional, Dict, Set

from src.util import mapjoin

SortOp = str # NewType('SortOp',str)
AtomicSort = str # NewType('AtomicSort',str)
Sort = Union[AtomicSort, 'NonatomicSort']
class NonatomicSort(NamedTuple):
    sortop: SortOp
    args_: Tuple[Any,...]
    @staticmethod
    def c(sortop:AtomicSort, args:Tuple[Any,...]) -> 'NonatomicSort':
        return NonatomicSort(sortop, args)
    @property
    def args(self) -> Tuple[Sort,...]:
        return cast(Tuple[Sort], self.args_)
    @staticmethod
    def mk(sortop:AtomicSort, args:Tuple[Sort,...]) -> 'NonatomicSort':
        return NonatomicSort(sortop, args)

    def __str__(self) -> str:
        if self.sortop == 'Tuple':
            return f"{mapjoin(str,self.args,'×')}"
        elif self.sortop == 'Rate':
            return f"{mapjoin(str,self.args,'/')}"
        elif self.sortop == 'Copy':
            return str(self.args[1])
        else:
            return f"{self.sortop}[{mapjoin(str,self.args,', ')}]"
    def __repr__(self) -> str:
        return str(self)