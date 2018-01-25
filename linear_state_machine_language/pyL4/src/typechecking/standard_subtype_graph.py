from itertools import chain

from src.independent.TransitivelyClosedDirectedGraph import TransitivelyClosedDirectedGraph
from src.typechecking.standard_sorts import *
from src.typechecking.SubsortConstraint import sschain, SubsortConstraint


SubsortGraph = TransitivelyClosedDirectedGraph[Sort]

def build_graph( subsort_constraints: Iterable[SubsortConstraint], initial_nodes:Iterable[Sort]) -> SubsortGraph:
    graph = SubsortGraph()
    graph.addNodes(initial_nodes)
    for constraint in subsort_constraints:
        graph.addEdge(constraint.parts[0],constraint.parts[1])
    add_derived(graph)
    # graph.addTop('Any')
    return graph

def add_derived(graph:SubsortGraph):
    for num1 in UnboundedNumericSorts:
        for num2 in UnboundedNumericSorts:
            if not graph.hasEdge(num1,num2):
                continue
            for den1 in [PosReal,PosInt]:
                for den2 in {PosReal, PosInt}:
                    if not graph.hasEdge(den1,den2):
                        continue
                    graph.addEdge(Ratio(num1,den1), Ratio(num2,den2))

    for S in AllAtomicSorts:
        # graph.addEdge(NonatomicSort('Tuple', (S, S)), NonatomicSort('Tuple', ('Any', 'Any')))
        for T in AllAtomicSorts:
            if not graph.hasEdge(S,T):
                continue
            graph.addEdge(SApp('Tuple',S,S), SApp('Tuple',T,T))

    for S in TDMapKeySorts:
        graph.addEdge('EmptyTDMap', SApp('TDMap',S))

    # for copied_sort in all_sort_copies_by_orig:
    #     for acopy in all_sort_copies_by_orig[copied_sort]:
    #         graph.addEdge(acopy, copied_sort)



SUBSORT_CONSTRAINTS : Iterable[SubsortConstraint] = chain(
    sschain(PosTimeDelta, TimeDelta),
    sschain("{0}","[0,1)"),
    sschain("{1}","(0,1]"),
    sschain("{0,1}","[0,1]"),
    sschain("{0}","{0,1}",Nat),
    sschain("{1}","{0,1}"),
    sschain("{1}",PosInt),
    sschain("(0,1)", "[0,1)", "[0,1]", NonnegReal),
    sschain("(0,1)", "(0,1]", "[0,1]", NonnegReal),
    sschain("(0,1)", "(0,1]", PosReal),
    sschain(PosInt,Nat,Int),
    sschain(PosReal,NonnegReal,Real),
    sschain(PosInt,PosReal),
    sschain(Nat,NonnegReal),
    sschain(Int,Real),
    sschain(Ratio(NonnegReal, PosReal), NonnegReal),
    sschain(Ratio(NonnegReal, PosInt), NonnegReal),
    sschain(Ratio(PosReal,PosReal), PosReal),
    sschain(Ratio(PosReal,PosInt), PosReal),
    sschain(Ratio(Real, PosReal), Real),
    sschain(Ratio(Real,PosInt), Real),

    sschain(Ratio(PosRealj, PosIntj), Ratio(NonnegRealj, PosIntj)),
    sschain(Ratio(PosRealj, PosRealj), Ratio(NonnegRealj, PosRealj)),

    # sschain(Ratio(NonnegRealj, PosRealj), NonnegRealj),
    # sschain(Ratio(NonnegRealj, PosIntj), NonnegRealj),
    # sschain(Ratio(PosRealj,PosRealj), PosRealj),
    # sschain(Ratio(PosRealj,PosIntj), PosRealj),

    sschain(PosIntj,Natj),
    sschain(PosRealj,NonnegReal)
)


STANDARD_SUBSORTING_GRAPH = build_graph(SUBSORT_CONSTRAINTS, AllSorts)
# print( standard_types_graph )