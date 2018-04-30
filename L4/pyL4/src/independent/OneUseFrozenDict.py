from typing import TypeVar, Dict

K = TypeVar('K')
V = TypeVar('V')

NoMutating = Exception("mutating not allowed")

class OneUseFrozenDict(Dict[K, V]):
    def __set__(self, k: K, v: V):
        raise NoMutating
    def clear(self):
        raise NoMutating
    def pop(self):
        raise NoMutating
    def popitem(self):
        raise NoMutating
    def update(self):
        raise NoMutating
