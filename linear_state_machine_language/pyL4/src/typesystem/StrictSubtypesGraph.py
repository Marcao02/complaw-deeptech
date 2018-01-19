from typing import Tuple, List, Dict, Set, Iterable, Optional, Iterator, Sequence

from copy import copy

from src.typesystem.Sorts import Sort
from src.util import mapjoin


class StrictSubtypesGraph:
    def __init__(self, sorts:Sequence[Sort], given_edges: List[Tuple[Sort,Sort]]) -> None:
        self.edges_from : Dict[Sort,Set[Sort]] = dict()
        for sort in sorts:
            self.edges_from[sort] = set()
        for (src,trg) in given_edges:
            self.addEdge(src,trg)

    def addEdge(self, src:Sort, trg:Sort) -> None:
        if src not in self.edges_from:
            self.edges_from[src] = {trg}
        else:
            self.edges_from[src].add(trg)
        if trg not in self.edges_from:
            self.edges_from[trg] = set()

    def hasEdge(self, srt:Sort, trg:Sort) -> bool:
        return trg in self.edges_from[srt]

    def simplifyIntersection(self, sorts:Set[Sort]) -> Optional[Sort]:
        reduced_set = copy(sorts)
        # print("simplifying", reduced_set)
        for u in sorts:
            for v in sorts:
                if v in reduced_set and u != v:
                    if self.hasEdge(u,v) and v in reduced_set:
                        # print("removing", str(v))
                        reduced_set.remove(v)
                    # else:
                    #     print(f"{u},{v} is not an edge")
        assert len(reduced_set) <= 1, "This sort set generated from code did not reduce to a single sort:\n" + str(reduced_set)
        return reduced_set.pop() if len(reduced_set) == 1 else None

    def transitivelyClose(self) -> None:
        for x in self.edges_from:
            self._addAncestorsOf(x)


    def _addAncestorsOf(self, u:Sort):
        assert u in self.edges_from

        stack : List[Sort] = []

        def pushParentsOntoStack(v:Sort) -> None:
            for par in self.edges_from[v]:
                stack.append(par)

        u_ancestors = self.edges_from[u]
        pushParentsOntoStack(u)
        while len(stack) > 0:
            w = stack.pop()
            for x in self.edges_from[w]:
                if x not in u_ancestors:
                    # print(f"Adding {u} ⊆ {x}")
                    self.addEdge(u,x)
                    stack.append(x)

    def __str__(self):
        rv = ""
        for src in self.edges_from:
            # rv += str(src) + " ⊆ "+ ", ".join(self.edges_from[src]) + "\n"
            rv += f"{src} ⊆ {mapjoin(str, self.edges_from[src], ',')}\n"
        return rv

