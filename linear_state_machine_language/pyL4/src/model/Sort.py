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

    def subst(self, var: str, val: Sort) -> Sort:
        return NonatomicSort.c(self.sortop, tuple(map(lambda s: sortsubst(s,var,val), self.args)))

    def substdict(self, d:Dict[str,Sort]) -> Sort:
        return NonatomicSort.c(self.sortop, tuple(map(lambda s: sortsubstdict(s,d), self.args)))

    def __str__(self) -> str:
        assert len(self.args) > 0
        if self.sortop == 'Tuple':
            return f"{mapjoin(str,self.args,'×')}"
        elif self.sortop == 'Ratio':
            return f"{mapjoin(str,self.args,'/')}"
        elif self.sortop == 'Dup':
            return str(self.args[0]) + f"[{self.args[1]}]"
        else:
            return f"{self.sortop}[{mapjoin(str,self.args,', ')}]"
    def __repr__(self) -> str:
        return str(self)

def sortsubst(intothis:Sort, var:str, val:Sort) -> Sort:
    if isinstance(intothis, NonatomicSort):
        return cast(Sort,intothis.subst(var,val))
    elif var == intothis:
        return val
    else:
        return intothis
def sortsubstdict(intothis:Sort, d:Dict[str,Sort]) -> Sort:
    if isinstance(intothis, NonatomicSort):
        return cast(Sort,intothis.substdict(d))
    elif intothis in d:
        return d[intothis]
    else:
        return intothis

