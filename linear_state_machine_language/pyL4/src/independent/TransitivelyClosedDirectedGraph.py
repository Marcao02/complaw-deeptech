from typing import Tuple, List, Dict, Set, Iterable, Optional, Iterator, Sequence, TypeVar, Union, Any, Callable, \
    Generic
from copy import copy


T = TypeVar('T')

class TransitivelyClosedDirectedGraphInvariantError(Exception):
    pass

"""
Can have cycles and loops
"""
class TransitivelyClosedDirectedGraph(Generic[T]):
    def __init__(self) -> None:
        self.edges_from : Dict[T,Set[T]] = dict()
        self.edges_from_inv: Dict[T, Set[T]] = dict()

    def hasNode(self, node:T) -> bool:
        return node in self.edges_from

    def addNode(self, node:T):
        self.addEdge(node,node)

    def addNodes(self, nodes:Iterable[T]):
        for node in nodes:
            self.addNode(node)

    def addEdge(self, src:T, trg:T):
        if src not in self.edges_from:
            self.edges_from[src] = {src}
            assert src not in self.edges_from_inv
            self.edges_from_inv[src] = {src}
        if trg not in self.edges_from:
            self.edges_from[trg] = {trg}
            assert trg not in self.edges_from_inv
            self.edges_from_inv[trg] = {trg}

        if src != trg:
            # if src in {"{0}","Nat","Real"} or trg in {"{0}","Nat","Real"}:
            #     print(f"\nbefore ({src},{trg})" +
            #           f"\n\tedges from {'{0}'}: {self.edges_from['{0}']}" +
            #           f"\n\tedges from Nat: {self.edges_from['Nat']}" +
            #           f"\n\tedges from {'{0,1}'}: {self.edges_from['{0,1}']}")
            if trg in self.edges_from[src]:
                # print(f"Edge {src} -> {trg} already in graph.")
                return
            else:
                self.edges_from[src].add(trg)
                self.edges_from_inv[trg].add(src)

                for ancestor_of_trg in self.edges_from[trg]:
                    for descendent_of_src in self.edges_from_inv[src]:
                        self.edges_from[descendent_of_src].add(ancestor_of_trg)
                        self.edges_from_inv[ancestor_of_trg].add(descendent_of_src)

            # if src in {"{0}", "Nat", "Real"} or trg in {"{0}", "Nat", "Real"}:
            #     print(f"after ({src},{trg})" +
            #           f"\n\tedges from {'{0}'}: {self.edges_from['{0}']}" +
            #           f"\n\tedges from Nat: {self.edges_from['Nat']}" +
            #           f"\n\tedges from {'{0,1}'}: {self.edges_from['{0,1}']}")

    def edgeIter(self) -> Iterator[Tuple[T,T]]:
        for src in self.edges_from:
            for trg in self.edges_from[src]:
                yield (src,trg)

    def edgeSet(self) -> Set[Tuple[T,T]]:
        rv = set()
        for src in self.edges_from:
            for trg in self.edges_from[src]:
                rv.add((src,trg))
        return rv

    def hasEdge(self, src:T, trg:T) -> bool:
        if src not in self.edges_from:
            raise Exception(f"{src} is not a node in the graph.")
        return trg in self.edges_from[src]

    # sub_nonexplicit takes care of structural subtyping rules
    def simplifyIntersection(self, nodes:Set[T], sub_nonexplicit:Callable[[T,T],bool]) -> Optional[T]:
        assert len(nodes) > 0
        if len(nodes) == 1:
            return nodes.pop()

        reduced_set = copy(nodes)
        # print("simplifying", reduced_set)
        for u in nodes:
            for v in nodes:
                if v in reduced_set and u != v:
                    if sub_nonexplicit(u,v) or self.hasEdge(u,v):
                        # print("removing", str(v))
                        reduced_set.remove(v)
        if len(reduced_set) > 1:
            print(self)
            raise TransitivelyClosedDirectedGraphInvariantError(f"The set {reduced_set} does not contain its lower bound.")
        return reduced_set.pop() if len(reduced_set) == 1 else None

    def addTop(self,top:T):
        self.addNode(top)
        for v in self.edges_from:
            self.edges_from[top].add(v)
            self.edges_from_inv[v].add(top)


    def __str__(self):
        rv = ""
        for src in self.edges_from:
            rv += f"{src} ≤ {mapjoin(str, self.edges_from[src], ', ')}\n"
        return rv


def mapjoin(f:Callable[[Any],str], things:Union[Iterable[Any],Iterator[Any]], delim:str='') -> str:
    return delim.join(map(f,things))