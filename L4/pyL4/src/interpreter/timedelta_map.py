from datetime import timedelta
from typing import Generic, TypeVar, Mapping, Iterable, Tuple, Any


"""
Inefficient immutable map from some type to timedelta, implemented as tuple of pairs 
"""

K = TypeVar('K')
TDMap = Tuple[Tuple[K,timedelta], ...]

def tdmapHas(tdmap:TDMap[K], key: K) -> bool:
    return any( (key == p[0] for p in tdmap) )

def tdmapHasItemExpiredBefore(tdmap:TDMap[K], td: timedelta) -> bool:
    return any( (p[1] < td for p in tdmap) )

def tdmapAdd(tdmap:TDMap[K], key: K, val:timedelta) -> TDMap[K]:
    assert not tdmapHas(tdmap, key)
    return tdmap + ((key,val),)

def tdmapGet(tdmap:TDMap[K], key: K) -> timedelta:
    assert tdmapHas(tdmap,key)
    for p in tdmap:
        if p[0] == key:
            return p[1]
    assert False

def tdmapDelete(tdmap:TDMap[K], key: K) -> TDMap[K]:
    assert tdmap is not None
    return tuple(filter(lambda p: p[0] != key, tdmap))

def tdmapSet(tdmap:TDMap, key:K, val:timedelta) -> TDMap[K]:
    assert tdmap is not None
    return tdmapAdd(tdmapDelete(tdmap,key), key, val)

# map has the key, and map's stored value is ≥ lower_bound
def tdmapTimeDeltaGEQ(tdmap:TDMap, key:K, lower_bound:timedelta) -> bool:
    return tdmapHas(tdmap, key) and tdmapGet(tdmap, key) >= lower_bound

# map has the key, and map's stored value is < upper_bound
def tdmapTimeDeltaLT(tdmap:TDMap, key:K, upper_bound:timedelta) -> bool:
    return tdmapHas(tdmap,key) and tdmapGet(tdmap,key) < upper_bound

#
# """
# Inefficient immutable maps, implemented as tuple of pairs
# """
# class ImmutableMap(tuple):
#     def set(self, key:KT, val:VT) -> Any: # returns a FrozenMap
#         pass
#
#     def delete(self, key:KT) -> Any:
#         pass
#
#     def has(self, key:KT) -> bool:
#         pass
#
#     def dateTimeLT(self, ):
    # def __init__(self, iter:Iterable) -> None:
    #
    #
    # def __iter__(self) -> Iterator[_T_co]:
    #
    # def __getitem__(self, k: _KT) -> _VT_co:
    #     pass
    #
    # def __len__(self) -> int:
    #     pass
