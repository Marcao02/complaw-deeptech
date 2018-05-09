from typing import Generic, Generator

from src.independent.util_for_tuple_linked_lists import TupleLinkedList, count, tllToStr
from src.independent.typing_imports import *

V = TypeVar('V')

class PushOnlyStack(Generic[V]):
    """An push-only (hence immutable) stack"""

    # nested tuples, forming a list in reverse order
    items : TupleLinkedList # the Any is being lazy with types

    def __init__(self, tll:Optional[TupleLinkedList] = None) -> None:
        self.items : TupleLinkedList = tll

    def push(self,v:V) -> 'PushOnlyStack':
        return PushOnlyStack((v,self.items))

    def count(self,v:V) -> int:
        return count(self.items,v) if self.items else 0

    def __str__(self) -> str:
        return tllToStr(self.items)

    def forEach(self, f:Callable[[V],None]):
        return tllForEach(self.items,f)

    # def __iter__(self):
    #     return tllIter(self.items)


def tllForEach(tll: TupleLinkedList, f:Callable[[V],None]):
    if tll:
        f(tll[0])
        tllForEach(tll[1],f)
# doesn't work
# def tllIter(tll:TupleLinkedList) -> Generator:
#     if tll:
#         yield tll[0]
#         tllIter(tll[1]).__iter__()
