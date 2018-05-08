from typing import Dict, Any, Set, TypeVar, Callable, Iterable, Tuple, List

T1 = TypeVar('T1')
T2 = TypeVar('T2')
T3 = TypeVar('T3')

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

SrcData = TypeVar('SrcData')
KeyData = TypeVar('KeyData')
def partitionBy(f:Callable[[SrcData],KeyData], collection:Iterable[SrcData]) -> Dict[str,List[SrcData]]:
    rv : Dict[str,List[SrcData]] = dict()
    for x in collection:
        key = str(f(x))
        if key not in rv:
            rv[key] = []
        rv[key].append(x)
    return rv

