from typing import NamedTuple, Tuple, Any, Union, cast, Dict

from src.independent.util import mapjoin

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

    def subst(self, var_or_sort: Sort, val: Sort) -> Sort:
        return NonatomicSort.c(self.sortop, tuple(map(lambda s: sortsubst(s,var_or_sort,val), self.args)))

    def substdict(self, d:Dict[Sort,Sort]) -> Sort:
        return NonatomicSort.c(self.sortop, tuple(map(lambda s: sortsubstdict(s,d), self.args)))

    def __str__(self) -> str:
        assert len(self.args) > 0
        if self.sortop == 'Tuple':
            return f"{mapjoin(str,self.args,'×')}"
        elif self.sortop == 'Ratio':
            return f"{mapjoin(str,self.args,'/')}"
        elif self.sortop == 'Dup':
            ind = 'j' if self.args[1] == 'jvar' else self.args[1]
            return str(self.args[0]) + f"[{ind}]"
        else:
            return f"{self.sortop}[{mapjoin(str,self.args,', ')}]"
    def __repr__(self) -> str:
        return str(self)

def sortsubst(intothis:Sort, var_or_sort:Sort, val:Sort) -> Sort:
    if isinstance(intothis, NonatomicSort):
        return cast(Sort,intothis.subst(var_or_sort,val))
    elif var_or_sort == intothis:
        return val
    else:
        return intothis

def sortsubstdict(intothis:Sort, d:Dict[Sort,Sort]) -> Sort:
    if intothis in d:
        # print(f"FOUND {intothis}")
        return d[intothis]
    elif isinstance(intothis, NonatomicSort):
        return cast(Sort,intothis.substdict(d))
    else:
        return intothis

