# from typing import Union, List, Dict, Any, Tuple, Callable
from typing import Iterator, List,  Set, Optional, NamedTuple

from model.SExpr import SExpr
from model.Term import Term
from model.constants_and_defined_types import *
from model.util import indent, mapjoin
from model.Connection import Connection, ConnectionToEnvAction


class Section:
    def __init__(self, section_id: SectionId) -> None:
        self.section_id = section_id
        self.visit_bounds: Optional[SExpr] = None
        self.section_description: Optional[str] = None
        self.prose_refs: List[str] = []

        self.preconditions: List[Term] = []

        self.connections_by_role: Dict[RoleId, List[Connection]] = dict()

        self.parent_action_id : Optional[ActionId] = None

    def is_anon(self) -> bool:
        return self.parent_action_id is not None

    # def vulnerableParties(self) -> List[RoleId]:
    #     print("BROKEN")
    #     return list(self.connections_by_role.keys())
    #
    def connections(self) -> Iterator[Connection]:
        for role_subset in self.connections_by_role.values():
            for t in role_subset:
                yield t

    def __str__(self):
        return self.toStr(0)

    def toStr(self,i:int) -> str:
        if self.parent_action_id is None:
            rv = indent(i) + f"section {self.section_id}:\n"
        else:
            rv = indent(i) + f"following section:\n"

        for pre in self.preconditions:
            rv += indent(i+1) + "pre: " + str(pre) + "\n"

        if self.section_description:
            rv += indent(i+1) + "description: " + self.section_description + "\n"

        # if self.visit_bounds:
        #     rv += indent(1) + "prove " + mapjoin(str, self.visit_bounds, " ") + "\n"

        for t in self.connections():
            rv += t.toStr(i+1) + "\n"

        return rv

    def __repr__(self) -> str:
        return str(self)