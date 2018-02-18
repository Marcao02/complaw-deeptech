from typing import NamedTuple, Tuple, Any, Union, cast, Dict, TYPE_CHECKING

from src.independent.util_for_str import mapjoin

SortOp = str # NewType('SortOp',str)
AtomicSort = str # NewType('AtomicSort',str)
Sort = Union[AtomicSort, 'SortOpApp']
# Sort = Union[AtomicSort, 'DataSortOpApp','CollectionSort']

class SortOpApp(NamedTuple):
    op: SortOp
    args_: Tuple[Any,...]

    @staticmethod
    def c(sortop:str, args:Tuple[Sort,...]) -> 'SortOpApp':
        return SortOpApp(sortop, args)
    @property
    def args(self) -> Tuple[Sort,...]:
        return cast(Tuple[Sort], self.args_)

    def subst(self, var_or_sort: Sort, val: Sort) -> Sort:
        return SortOpApp.c(self.op, tuple(map(lambda s: sortsubst(s, var_or_sort, val), self.args))) # type:ignore

    def substdict(self, d:Dict[Sort,Sort]) -> Sort:
        return SortOpApp.c(self.op, tuple(map(lambda s: sortsubstdict(s, d), self.args))) # type:ignore

    def complexity(self) -> int:
        return 1 + sum(sort_complexity(a) for a in self.args)

    def __str__(self) -> str:
        assert len(self.args) > 0
        if self.op == 'Tuple':
            return f"{mapjoin(str,self.args,'×')}"
        elif self.op == 'Ratio':
            return f"{mapjoin(str,self.args,'/')}"
        # elif self.op == 'Dimensioned':
        #     ind = 'i' if self.args[1] == 'dupvar' else self.args[1]
        #     return str(self.args[0]) + f"[{ind}]"
        elif self.op == 'Dimensioned':
            ind = 'i' if self.args[1] == 'dupvar' else self.args[1]
            return f"{self.args[1]}.{self.args[0]}"
        else:
            return f"{self.op}[{mapjoin(str,self.args,', ')}]"
    def __repr__(self) -> str:
        return str(self)

# class DataSortOpApp(SortOpApp):
#     pass
#
# DataSort = Union[DataSortOpApp,AtomicSort]
#
# class CollectionSort(SortOpApp):
#     pass

def sortsubst(intothis:Sort, var_or_sort:Sort, val:Sort) -> Sort:
    if isinstance(intothis, SortOpApp):
        return cast(Sort,intothis.subst(var_or_sort,val))
    elif var_or_sort == intothis:
        return val
    else:
        return intothis

def sortsubstdict(intothis:Sort, d:Dict[Sort,Sort]) -> Sort:
    if intothis in d:
        # print(f"FOUND {intothis}")
        return d[intothis]
    elif isinstance(intothis, SortOpApp):
        return cast(Sort,intothis.substdict(d))
    else:
        return intothis

def sort_complexity(s:Sort) -> int:
    if isinstance(s,str):
        return 1
    else:
        return s.complexity()


# SortOpPat = str # NewType('SortOp',str)
# SortPat = Union[AtomicSort, 'SortOpAppPat', SortMatchVar]
# class SortOpAppPat(NamedTuple):
#     op: SortOp
#     args_: Tuple[Any,...]
#
#     @staticmethod
#     def c(sortop:str, args:Tuple[SortPat,...]) -> 'SortOpAppPat':
#         return SortOpAppPat(sortop, args)
#     @property
#     def args(self) -> Tuple[Sort,...]:
#         return cast(Tuple[Sort], self.args_)
#
#     def substForVar(self, var_or_sort: Sort, val: Sort) -> SortPat:
#         return SortOpAppPat.c(self.op, tuple(map(lambda s: sortsubst(s, var_or_sort, val), self.args))) # type:ignore
#
#     def substdict(self, d:Dict[Sort,Sort]) -> SortPat:
#         return SortOpAppPat.c(self.op, tuple(map(lambda s: sortsubstdict(s, d), self.args))) # type:ignore
#
#     def complexity(self) -> int:
#         return 1 + sum(sort_complexity(a) for a in self.args)
#
#     def __str__(self) -> str:
#         assert len(self.args) > 0
#         if self.op == 'Tuple':
#             return f"{mapjoin(str,self.args,'×')}"
#         elif self.op == 'Ratio':
#             return f"{mapjoin(str,self.args,'/')}"
#         # elif self.op == 'Dimensioned':
#         #     ind = 'i' if self.args[1] == 'dupvar' else self.args[1]
#         #     return str(self.args[0]) + f"[{ind}]"
#         elif self.op == 'Dimensioned':
#             ind = 'i' if self.args[1] == 'dupvar' else self.args[1]
#             return f"{self.args[1]}.{self.args[0]}"
#         else:
#             return f"{self.op}[{mapjoin(str,self.args,', ')}]"
#     def __repr__(self) -> str:
#         return str(self)
