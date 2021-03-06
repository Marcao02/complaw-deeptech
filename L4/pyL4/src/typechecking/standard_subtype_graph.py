from itertools import chain

from src.independent.TransitivelyClosedDirectedGraphWithUnions import TransitivelyClosedDirectedGraphWithUnions
from src.typechecking.standard_sorts import *
from src.typechecking.SubsortConstraint import sschain, SubsortConstraint


SubsortGraph = TransitivelyClosedDirectedGraphWithUnions[Sort]

def build_graph( subsort_constraints: Iterable[SubsortConstraint], initial_nodes:Iterable[Sort]) -> SubsortGraph:
    graph = SubsortGraph()
    graph.addNodes(initial_nodes)
    for constraint in subsort_constraints:
        graph.addEdge(constraint.parts[0],constraint.parts[1])
    add_derived(graph)
    # graph.addTop('Any')

    for pair in UNIONS:
        graph.informOfUnion(pair[0],pair[1])

    return graph


"""
If s1 ⊆ s2 is in graph, and substForVar maps some sorts to sorts, add s1[substForVar] ⊆ s2[substForVar] to graph. 
"""
def duplicate_some_edges(subst:Dict[Sort,Sort], graph:SubsortGraph):
    orig_edges = graph.edgeSet()
    for src,trg in orig_edges:
        src_new = sortsubstdict(src,subst)
        trg_new = sortsubstdict(trg,subst)
        if src != src_new or trg != trg_new:
            graph.addEdge(src_new,trg_new)

# These can instead be done at typechecking time and cached.
def add_derived(graph:SubsortGraph):
    # Ratio
    for num1 in UnboundedNumericSorts:
        for num2 in UnboundedNumericSorts:
            if not graph.hasEdge(num1,num2):
                continue
            for den1 in [PosReal,PosInt]:
                for den2 in [PosReal, PosInt]:
                    if not graph.hasEdge(den1,den2):
                        continue
                    graph.addEdge(Ratio(num1,den1), Ratio(num2,den2))

    # Tuple - moved to typecheck.sub
    # for s1 in AtomicSortsAndDimensionedNumericSorts:
    #     # graph.addEdge(SortOpApp('Tuple', (S, S)), SortOpApp('Tuple', ('Any', 'Any')))
    #     for s2 in AtomicSortsAndDimensionedNumericSorts:
    #         if not graph.hasEdge(s1,s2):
    #             continue
    #         graph.addEdge(SApp('Tuple',s1,s1), SApp('Tuple',s2,s2))

    # EmptyTDMap - moved to typecheck.sub
    # for s in TDMapKeySorts:
    #     graph.addEdge('EmptyTDMap', SApp('TDMap',s))

SUBSORT_CONSTRAINTS : Iterable[SubsortConstraint] = chain(
    sschain(PosTimeDelta, TimeDelta),

    sschain(PosInt,Nat,Int),
    sschain(PosReal,NonnegReal,Real),
    sschain(PosInt,PosReal),
    sschain(Nat,NonnegReal),
    sschain(Int,Real),

    sschain("{0,1}",Nat),
    sschain("{1}",PosInt),
    sschain("{0}","{0,1}"),
    sschain("{1}","{0,1}"),
    sschain("{0}","Fraction[0,1)"),
    sschain("{1}","Fraction(0,1]"),
    sschain("{0,1}","Fraction[0,1]"),
    sschain("Fraction[0,1]",NonnegReal),
    sschain("Fraction(0,1)", "Fraction(0,1]", PosReal),
    sschain("Fraction(0,1)", "Fraction[0,1)", "Fraction[0,1]"),
    sschain("Fraction(0,1)", "Fraction(0,1]", "Fraction[0,1]"),

    # Ratios of non-dimensioned numeric sorts
    sschain(Ratio(NonnegReal, PosReal), NonnegReal),
    sschain(Ratio(NonnegReal, PosInt), NonnegReal),
    sschain(Ratio(PosReal, PosReal), PosReal),
    sschain(Ratio(PosReal, PosInt), PosReal),
    sschain(Ratio(Real, PosReal), Real),
    sschain(Ratio(Real, PosInt), Real),

    # We don't want all the duplicate copies of numeric types to have all the relations that the original numeric types have.
    # For now, we only need or want these:
    sschain(PosInt1, Nat1), # makes sense for counting any kind of thing
    sschain(PosReal2, NonnegReal2), # makes sense for measuring any kind of thing

    sschain(Nat1, Int1),  # unfortunately currently needed for subtraction
    sschain(NonnegReal2, Real2),  # unfortunately currently needed for subtraction

    # makes sense as consequence of previous two lines:
    sschain(Ratio(PosReal2, PosInt1), Ratio(NonnegReal2, PosInt1)),
    # not using this yet, but it's fine:
    sschain(Ratio(PosReal2, PosReal2), Ratio(NonnegReal2, PosReal2)),

    sschain("Fraction[0,1]", Ratio(Nat,PosInt))
)

UNIONS : Tuple[Tuple[Set[Sort],Sort],...] = (
    ({'{0}', PosReal}, NonnegReal),
    ({'{0}', PosInt}, Nat),
    ({'{0}', '{1}'}, '{0,1}'),
    ({'Fraction(0,1]', '{0}'}, 'Fraction[0,1]'),
    ({'Fraction[0,1)', '{1}'}, 'Fraction[0,1]'),
    ({'Fraction(0,1)', '{0}'}, 'Fraction[0,1)'),
    ({'Fraction(0,1)', '{1}'}, 'Fraction(0,1]'),
    ({'Fraction(0,1)', '{0,1}'}, 'Fraction[0,1]'),
)

DERIVED_UNIONS = (
    # can't actually derive this yet, but derivation using other identities would be:
    # (0,1] ⋃ [0,1) = ((0,1) ⋃ {1}) ⋃ ((0,1) ⋃ {0}) = (0,1) ⋃ {0,1} = [0,1]
    ({'Fraction(0,1]','Fraction[0,1)'}, 'Fraction[0,1]'),
)


STANDARD_SUBSORTING_GRAPH = build_graph(SUBSORT_CONSTRAINTS, AllSorts)
# print( STANDARD_SUBSORTING_GRAPH )

