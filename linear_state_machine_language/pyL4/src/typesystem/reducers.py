from typing import Any, List, Tuple, NamedTuple, Dict, Sequence, Optional, Union, NewType, Set, cast

from src.typesystem.Sorts import *
from src.typesystem.SubtypesGraph import SubtypesGraph


def build_graph() -> SubtypesGraph:
    subtype_pairs = []
    # line: Tuple[str,...]
    for line in subtypes_data:
        for i in range(len(line) - 1):
            subtype_pairs.append((line[i],line[i+1]))
    graph = SubtypesGraph(subtype_pairs)
    # print(graph)
    graph.transitivelyClose()
    return graph


def eliminate_type_vars(overloaded_types_data:Sequence[ Tuple[Sequence[str], Any] ]) -> Dict[str, List[Sequence[Any]]]:
    fntypes_map_ : Dict[str, List[Sequence[Any]]] = dict()
    for pair in overloaded_types_data:
        fst = pair[0]
        snd : Any = pair[1]
        symbs : Sequence[str] = cast(Sequence[str], (fst,) if isinstance(fst,str) else fst)
        fntypes = cast(List[Sequence[Any]], [snd] if (snd[0] == 'fn' or snd[0] == 'aafn') else list(snd))
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

# def eliminate_subtyping()
def print_types_map(fntypes_map:Dict[str, List[Sequence[Any]]]):
    for symb in fntypes_map:
        print(symb)
        print("\t", str(fntypes_map[symb]))
    print(sum(len(fntypes_map[f]) for f in fntypes_map), "simple function types total.")
