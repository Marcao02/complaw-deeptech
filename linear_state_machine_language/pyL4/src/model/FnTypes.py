from src.independent.util import mapjoin, todo_once
from src.independent.typing_imports import *
from src.model.Sort import Sort, sortsubst, sortsubstdict

SortTuple = Tuple[Sort,...]

class SimpleFnType(NamedTuple):
    parts: SortTuple
    @property
    def dom(self) -> SortTuple:
        return self.parts[:-1]
    @property
    def ran(self) -> Sort:
        return self.parts[-1]

    def subst(self,var_or_sort:Sort,val:Sort) -> 'SimpleFnType':
        return SimpleFnType(tuple(map(lambda s: cast(Sort,sortsubst(s, var_or_sort, val)), self.parts)))

    def substdict(self,d:Dict[Sort,Sort]) -> 'SimpleFnType':
        return SimpleFnType(tuple(map(lambda s: cast(Sort,sortsubstdict(s, d)), self.parts)))

    def __str__(self) -> str:
        todo_once("contrib: why cast necessary?")
        return mapjoin(str,self.parts,' ⟶ ')
    def __repr__(self) -> str:
        return str(self)

# class ArbArityFnType(NamedTuple):
#     dom: Sort
#     ran: Sort
#
#     def subst(self,var_or_sort:Sort,val:Sort) -> 'ArbArityFnType':
#         return ArbArityFnType(sortsubst(self.dom,var_or_sort,val), sortsubst(self.ran,var_or_sort,val))
#     def substdict(self,d:Dict[Sort,Sort]) -> 'ArbArityFnType':
#         return ArbArityFnType(sortsubstdict(self.dom, d),sortsubstdict(self.ran, d))
#
#     @property
#     def parts(self) -> Iterable[Sort]:
#         yield self.dom
#         yield self.ran
#
#     def __str__(self) -> str:
#         return f"{self.dom}* ⟶ {self.ran}"
#     def __repr__(self) -> str:
#         return str(self)

# NonoverloadedFnType = Union[SimpleFnType,ArbArityFnType]
NonoverloadedFnType = SimpleFnType

class OverloadedFnType(NamedTuple):
    parts: Set[NonoverloadedFnType]
    range_memo: Dict[SortTuple, Optional[Sort]]

    def add_substdict_copies(self,d:Dict[Sort,Sort]):
        have = set(self.parts)
        for noft in self.parts:
            new_noft = noft.substdict(d)
            if new_noft not in have:
                have.add(new_noft)
        self.parts.clear()
        self.parts.update(have)

    def replace_sorts(self,d:Dict[Sort,Sort]):
        have = set(self.parts)
        for noft in self.parts:
            new_noft = noft.substdict(d)
            if new_noft != noft:
                have.add(new_noft)
                if noft in have:
                    have.remove(noft)

        self.parts.clear()
        self.parts.update(have)

    def __str__(self) -> str:
        return "\t" + mapjoin(str, self.parts, "\n\t")

class FilteredOverloadedFnType(OverloadedFnType):
    @property
    def parts_sft(self) -> Set[SimpleFnType]:
        return self.parts