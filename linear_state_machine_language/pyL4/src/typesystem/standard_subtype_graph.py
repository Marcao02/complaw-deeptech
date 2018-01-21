from typing import Tuple, Iterable, Set

from src.independent.TransitivelyClosedDirectedGraph import TransitivelyClosedDirectedGraph
from src.typesystem.Sorts import *


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
                    graph.addEdge(SApp('Rate',num1,den1), SApp('Rate',num2,den2))

    for S in AllAtomicSorts:
        # graph.addEdge(NonatomicSort('Tuple', (S, S)), NonatomicSort('Tuple', ('Any', 'Any')))
        for T in AllAtomicSorts:
            if not graph.hasEdge(S,T):
                continue
            graph.addEdge(SApp('Tuple',S,S), SApp('Tuple',T,T))

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
    (SApp('Rate',NonnegReal, PosReal), NonnegReal),
    (SApp('Rate',NonnegReal, PosInt), NonnegReal),
    (SApp('Rate',PosReal,PosReal), PosReal),
    (SApp('Rate',Real,PosReal), Real),
    (SApp('Rate',PosReal,PosInt), PosReal),
    (SApp('Rate',Real,PosInt), Real),
)

standard_types_graph = build_graph(subtypes_data, AllSorts)
print( standard_types_graph )