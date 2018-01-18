from typing import Any, List, Tuple, NamedTuple, Dict, Sequence, Optional, Union, NewType, Set, cast

from src.typesystem.Sorts import *
from typesystem.SubtypesGraph import SubtypesGraph


def build_graph() -> SubtypesGraph:
    subtype_pairs = []
    for line in subtypes_data:
        for i in range(len(line) - 1):
            subtype_pairs.append((line[i],line[i+1]))
    graph = SubtypesGraph(subtype_pairs)
    # print(graph)
    graph.transitivelyClose()
    return graph


def eliminate_type_vars() -> Dict[str, List[Sequence[Any]]]:
    from src.typesystem.standard_types import overloaded_types_data

    fntypes_map_ : Dict[str, List[Sequence[Any],...]] = dict()
    for pair in overloaded_types_data:
        fst = pair[0]
        snd : Any = pair[1]
        symbs : Sequence[str] = cast(Sequence[str], (fst,) if isinstance(fst,str) else fst)
        fntypes = cast(List[Sequence[Any]], list((snd,) if (snd[0] == 'fn' or snd[0] == 'aafn') else list(snd)))
        for symb in symbs:
            if symb not in fntypes_map_:
                fntypes_map_[symb] = list(fntypes)
            else:
                fntypes_map_[symb] = fntypes_map_[symb] + fntypes

    return fntypes_map_


def eliminate_unbounded_arity(arity_occurrences: Dict[str,Set[int]], fntypes_map: Dict[str, List[Sequence[Any]]]) -> None:
    for f in fntypes_map:
        # if len(arity_occurrences[f]) == 0:
        #     continue

        ftypes = fntypes_map[f]
        for i in range(len(ftypes)-1,-1,-1):
            ftype = ftypes[i]
            if ftype[0] != 'aafn':
                continue
            assert len(ftype) == 3
            del ftypes[i]
            if f in arity_occurrences:
                dom = ftype[1]
                ran = ftype[2]
                for arity in arity_occurrences[f]:
                    ftypes.append(('fn',) + (dom,)*arity + (ran,))

fntypes_map = eliminate_type_vars()

def print_types_map():
    for symb in fntypes_map:
        print(symb)
        print("\t", str(fntypes_map[symb]))
print_types_map()

graph = build_graph()
print("\n" + str(graph))

eliminate_unbounded_arity({'and*':{3}, '*':{2,3}, 'max':{2,3}, 'min':{2},
                           '≤':{2,3}, '<':{2,3}, '>':{2,3},
                           '≥':{2,3}, '==':{2}, '+':{2,3}}, fntypes_map)
print_types_map()
