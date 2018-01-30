from typing import Dict

from src.independent.util import todo_once
from src.model.Sort import Sort, sortsubstdict
from src.model.FnTypes import OverloadedFnType
from src.typechecking.standard_sorts import NonnegReal, SApp, PosReal, jvar, Nat, PosInt
from src.typechecking.standard_subtype_graph import SubsortGraph

FnTypesMap = Dict[str, OverloadedFnType]


def doit_for_safe(fntypes_map:FnTypesMap, graph:SubsortGraph):
    Dup = lambda x,y: SApp("Dup",x,y)
    subst :Dict[Sort,Sort] = {
        Dup(NonnegReal, jvar): Dup(NonnegReal, '$'),
        Dup(PosReal, jvar): Dup(PosReal, 'Pos$'),
        Dup(Nat, jvar): Dup(Nat, 'ShareCnt'),
        Dup(PosInt, jvar): Dup(PosInt, 'PosShareCnt')
    }

    for fsymb,oft in fntypes_map.items():
        oft.replace_sorts(subst)

    for sort in subst.values():
        graph.addNode(sort)

    duplicate_some_edges(subst,graph)

    todo_once("NEED TO SUBSTITUTE NODES IN SUBSORTING GRAPH TOO. For now manually adding")
    graph.addEdge(Dup(PosReal,'Pos$'), Dup(NonnegReal,'$'))
    graph.addEdge(Dup(PosInt, 'PosShareCnt'), Dup(Nat, 'ShareCnt'))

def duplicate_some_edges(subst:Dict[Sort,Sort], graph:SubsortGraph):
    orig_edges = graph.edgeSet()
    for src,trg in orig_edges:
        src_new = sortsubstdict(src,subst)
        trg_new = sortsubstdict(trg,subst)
        if src != src_new or trg != trg_new:
            graph.addEdge(src_new,trg_new)