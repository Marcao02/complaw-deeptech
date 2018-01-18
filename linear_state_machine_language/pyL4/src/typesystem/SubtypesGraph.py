from typing import Tuple, List, Dict, Set, Iterable, Optional

from typesystem.Sorts import Sort


class SubtypesGraph:
    def __init__(self, given_edges: List[Tuple[Sort,Sort]]) -> None:
        self.edges_from : Dict[Sort,Set[Sort]] = dict()
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

    def simplifyIntersection(self, sorts:Iterable[Sort]) -> Optional[Sort]:
        sorts_set = set(sorts)
        for u in sorts:
            for v in sorts:
                if u != v and self.hasEdge(u,v):
                    sorts_set.remove(v)
        assert len(sorts_set) <= 1, "This sort set generated from code did not reduce to a single sort:\n" + str(sorts_set)
        return sorts_set.pop() if len(sorts_set) == 1 else None

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
            rv += str(src) + " ⊆ "+ ", ".join(self.edges_from[src]) + "\n"
        return rv

