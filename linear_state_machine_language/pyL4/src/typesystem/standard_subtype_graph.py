from typing import Tuple, Iterable, Set

from src.independent.TransitivelyClosedDirectedGraph import TransitivelyClosedDirectedGraph
from src.typesystem.standard_sorts import *


def build_graph(_subtypes_data:Tuple[Tuple[Sort,...],...], initial_nodes:Iterable[Sort]) -> TransitivelyClosedDirectedGraph[Sort]:
    graph = TransitivelyClosedDirectedGraph[Sort]()
    graph.addNodes(initial_nodes)
    for line in _subtypes_data:
        for i in range(len(line) - 1):
            graph.addEdge(line[i],line[i+1])
    add_derived(graph)
    # graph.addTop('Any')
    return graph

def add_derived(graph:TransitivelyClosedDirectedGraph[Sort]):
    for num1 in UnboundedNumericSorts:
        for num2 in UnboundedNumericSorts:
            if not graph.hasEdge(num1,num2):
                continue
            for den1 in [PosReal,PosInt]:
                for den2 in {PosReal, PosInt}:
                    if not graph.hasEdge(den1,den2):
                        continue
                    graph.addEdge(SApp('Ratio',num1,den1), SApp('Ratio',num2,den2))

    for S in AllAtomicSorts:
        # graph.addEdge(NonatomicSort('Tuple', (S, S)), NonatomicSort('Tuple', ('Any', 'Any')))
        for T in AllAtomicSorts:
            if not graph.hasEdge(S,T):
                continue
            graph.addEdge(SApp('Tuple',S,S), SApp('Tuple',T,T))

    for S in TDMapKeySortData:
        graph.addEdge('EmptyTDMap', SApp('TDMap',S))

    # for copied_sort in all_sort_copies_by_orig:
    #     for acopy in all_sort_copies_by_orig[copied_sort]:
    #         graph.addEdge(acopy, copied_sort)

subtypes_data : Tuple[Tuple[Sort,...],...] = (
    (PosTimeDelta, TimeDelta),
    ("{0}","[0,1)"),
    ("{1}","(0,1]"),
    ("{0,1}","[0,1]"),
    ("{0}","{0,1}",Nat),
    ("{1}","{0,1}"),
    ("{1}",PosInt),
    ("(0,1)", "[0,1)", "[0,1]", NonnegReal),
    ("(0,1)", "(0,1]", "[0,1]", NonnegReal),
    ("(0,1)", "(0,1]", PosReal),
    (PosInt,Nat,Int),
    (PosReal,NonnegReal,Real),
    (PosInt,PosReal),
    (Nat,NonnegReal),
    (Int,Real),
    (SApp('Ratio',NonnegReal, PosReal), NonnegReal),
    (SApp('Ratio',NonnegReal, PosInt), NonnegReal),
    (SApp('Ratio',PosReal,PosReal), PosReal),
    (SApp('Ratio',Real,PosReal), Real),
    (SApp('Ratio',PosReal,PosInt), PosReal),
    (SApp('Ratio',Real,PosInt), Real),
)

standard_types_graph = build_graph(subtypes_data, AllSorts)
print( standard_types_graph )