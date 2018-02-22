from src.independent.TransitivelyClosedDirectedGraph import *

"""
Can have cycles and loops
Graph should have a node for every non-empty intersection/meet/glb (or every that you might encounter). 
Need not have nodes for unions/joins/lubs. 
mypy question: does it need to be Generic[T]?
"""
class TransitivelyClosedDirectedGraphWithUnions(TransitivelyClosedDirectedGraph[T]):

    def __init__(self) -> None:
        super().__init__()
        # maybe later, complicates simplifyUnion:
        # self.joins: Dict[T, Tuple[T,...]] = dict()
        # self.joins_inv: Dict[Tuple[T,...], T] = dict()
        self.unions: Dict[T, Set[T]] = dict()
        self.unions_inv: Dict[Set[T], T] = dict()

    def informOfUnion(self, parts:Set[T], union_of_parts:T):
        assert len(parts) == 2, "only unions of two sorts supported at the moment. you'll have to create intermediates manually, for now."
        for part in parts:
            assert union_of_parts in self.edges_from[part], \
                f"You must tell the graph that {union_of_parts} ≥ {part} " \
                f"before you can tell it that {union_of_parts} = {mapjoin(str,parts,'⋃')}"
        self.unions[union_of_parts] = parts
        self.unions_inv[parts] = union_of_parts

    def simplifyUnion(self, nodes:Set[T]) -> Optional[T]:
        assert len(nodes) > 0
        if len(nodes) == 1:
            return nodes.pop()

        changes = 1
        reduced_set = copy(nodes)
        while changes > 0:
            changes = 0
            for u1 in nodes:
                if u1 not in reduced_set:
                    continue
                for u2 in nodes:
                    if u2 not in reduced_set or u1 == u2:
                        continue
                    if {u1,u2} in self.unions_inv:
                        u1u2 = self.unions_inv[{u1, u2}]
                        reduced_set.difference_update({u1,u2})
                        reduced_set.add(u1u2)
                        changes += 1

        if len(reduced_set) > 1:
            print(self)
            raise TransitivelyClosedDirectedGraphInvariantError(
                f"The graph does not know about the union/join/lub of {reduced_set}.")
        return reduced_set.pop() if len(reduced_set) == 1 else None


