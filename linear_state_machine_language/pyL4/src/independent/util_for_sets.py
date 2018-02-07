from typing import Any, List, Dict, Union, Sequence, TypeVar, Set, Iterable, Tuple, FrozenSet

V = TypeVar('V')

def fset(*args:V) -> FrozenSet[V]:
    return frozenset(args)

print( fset(1,2,3,4) )