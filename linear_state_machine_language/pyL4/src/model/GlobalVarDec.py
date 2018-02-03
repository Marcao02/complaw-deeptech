from itertools import chain

from src.independent.typing_imports import *

from src.constants_and_defined_types import StateVarId
from src.model.Term import Term
from src.model.Sort import Sort


class GlobalVarDec(NamedTuple):
    name: StateVarId
    sort: Sort
    initval: Optional[Term]
    modifier:List[str]

    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        rviter : Iterable[T] = iteraccum_maybe or []
        if self.initval:
            rviter = chain(rviter, f(self.initval))
        return rviter

    def forEach(self, pred: Callable[[Any], bool], f: Callable[[Any], Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        if self.initval:
            rviter = chain(rviter, self.initval.forEach(pred, f))
        return rviter

    def isWriteOnceMore(self) -> bool:
        return 'writeOnceMore' in self.modifier

    def __str__(self) -> str:
        return (' '.join(self.modifier) + ' ' if self.modifier else '') + \
               self.name + " : " + str(self.sort) + \
               (" := " + str(self.initval) if (self.initval is not None) else '')
