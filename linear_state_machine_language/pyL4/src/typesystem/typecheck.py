from typing import Union

from typesystem.FnTypes import OverloadedFnType, SortTuple, SimpleFnType, ArbArityFnType, Optional
from typesystem.Sorts import Sort
from typesystem.SubtypesGraph import SubtypesGraph
from typesystem.standard_types import standard_types_graph

graph : SubtypesGraph = standard_types_graph

def overloaded_fnapp_range_memo(oft:OverloadedFnType, argsorts:SortTuple) -> Optional[Sort]:
    rangeset = filter(lambda x: x is not None, (ft_range(ft, argsorts) for ft in oft.parts))
    intersection = graph.simplifyIntersection(rangeset)
    if intersection:
        oft.range_memo[argsorts] = intersection
        return intersection
    else:
        oft.illtyped_memo.add(argsorts)
        return None

def ft_range(fntype:Union[SimpleFnType,ArbArityFnType], argsorts:SortTuple) -> Optional[Sort]:
    return sft_range(fntype, argsorts) if isinstance(fntype, SimpleFnType) else aaft_range(fntype, argsorts)

def sft_range(fntype:SimpleFnType, argsorts:SortTuple) -> Optional[Sort]:
    if len(fntype.parts) != len(argsorts):
        return None
    for i in range(len(argsorts)):
        argsort = argsorts[i]
        fnsort = fntype.parts[i]
        if not graph.hasEdge(argsort,fnsort):
            return None
    return fntype.ran

def aaft_range(fntype:ArbArityFnType, argsorts:SortTuple) -> Optional[Sort]:
    for i in range(len(argsorts)):
        argsort = argsorts[i]
        if not graph.hasEdge(argsort,fntype.dom):
            return None
    return fntype.ran