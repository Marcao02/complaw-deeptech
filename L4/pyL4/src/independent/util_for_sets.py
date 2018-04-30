from typing import TypeVar, FrozenSet

V = TypeVar('V')

def fset(*args:V) -> FrozenSet[V]:
    return frozenset(args)




