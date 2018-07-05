from datetime import timedelta
from typing import Generic, TypeVar, Mapping, Iterable, Tuple, Any, Union, FrozenSet, cast

"""
Inefficient immutable map from some type to timedelta, implemented as tuple of pairs 
"""

K = TypeVar('K')
TDMap = Tuple[Tuple[K,timedelta], ...]
Collection = Union[TDMap,FrozenSet[K]]

def colHas(col: Collection, key: K) -> bool:
    if isinstance(col,tuple):
        return any( (key == p[0] for p in col) )
    else:
        return key in col

def tdmapMinValue(tdmap:TDMap[K]) -> timedelta:
    return min( p[1] for p in tdmap )

def tdmapHasItemExpiredBefore(tdmap:TDMap[K], td: timedelta) -> bool:
    return any( (p[1] < td for p in tdmap) )

# def mapAdd(col:TDMap[K], key: K, val:timedelta) -> TDMap[K]:
#     assert not colHas(col, key)
#     return col + ((key,val),)

def tdmapGet(tdmap:TDMap[K], key: K) -> timedelta:
    assert colHas(tdmap,key)
    for p in tdmap:
        if p[0] == key:
            return p[1]
    assert False

def deleteFromCollection(col: Collection, key: K) -> Collection:
    assert col is not None
    if isinstance(col, tuple):
        return tuple(filter(lambda p: p[0] != key, col))
    elif isinstance(col, FrozenSet):
        return col.difference({key})
    assert False


def tdmapSet(tdmap:TDMap[K], key:K, val:timedelta) -> TDMap[K]:
    assert tdmap is not None
    return tdmapAdd(cast(TDMap[K],deleteFromCollection(tdmap,key)), key, val)

def tdmapAdd(tdmap:TDMap[K], key: K, val:timedelta) -> TDMap[K]:
    assert not colHas(tdmap, key)
    return tdmap + ((key,val),)

# map has the key, and map's stored value is ≥ lower_bound
def tdmapTimeDeltaGEQ(tdmap:TDMap, key:K, lower_bound:timedelta) -> bool:
    return colHas(tdmap, key) and tdmapGet(tdmap, key) >= lower_bound

def tdmapTimeDeltaLEQ(tdmap:TDMap, key:K, upper_bound:timedelta) -> bool:
    return colHas(tdmap, key) and tdmapGet(tdmap, key) <= upper_bound

# map has the key, and map's stored value is < upper_bound
def tdmapTimeDeltaLT(tdmap:TDMap, key:K, upper_bound:timedelta) -> bool:
    return colHas(tdmap,key) and tdmapGet(tdmap,key) < upper_bound

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
