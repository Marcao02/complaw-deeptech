from typing import Dict, Any, Set, TypeVar


T1 = TypeVar('T1')
T2 = TypeVar('T2')

def dictSetAdd(d:Dict[T1, Set[T2]], key:T1, val:T2) -> None:
    if key in d:
        d[key].add(val)
    else:
        d[key] = {val}

def dictSetGet(d:Dict[T1, Set[T2]], key:T1) -> Set[T2]:
    return d[key] if key in d else set()

def dictInc(d:Dict[T1, int], key:T1, init:int) -> None:
    if key in d:
        d[key] += 1
    else:
        d[key] = init


def hasNotNone(d:Dict[T1,Any], x:T1) -> bool:
    return (x in d) and not(d[x] is None)