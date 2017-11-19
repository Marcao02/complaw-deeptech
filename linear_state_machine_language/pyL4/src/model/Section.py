# from typing import Union, List, Dict, Any, Tuple, Callable
from typing import Iterator, List,  Set, Optional, NamedTuple

from model.SExpr import SExpr
from model.constants_and_defined_types import *
from model.util import indent, mapjoin
from model.Connection import Connection, ConnectionToEnvAction


class Section:
    def __init__(self, section_id: str) -> None:
        self.section_id = section_id

        self.visit_bounds: Optional[SExpr] = None

        self.connections_by_role: Dict[RoleId, List[Connection]] = dict()

        self.section_description: Optional[str] = None

        self.is_compound = False

        self.prose_refs: List[str] = []

    # def vulnerableParties(self) -> List[RoleId]:
    #     print("BROKEN")
    #     return list(self.connections_by_role.keys())
    #
    def connections(self) -> Iterator[Connection]:
        for role_subset in self.connections_by_role.values():
            for t in role_subset:
                yield t

    def __str__(self) -> str:
        rv = f"section {self.section_id}:\n"

        if self.section_description:
            rv += indent(1) + "description: " + self.section_description

        if self.visit_bounds:
            rv += indent(1) + "prove " + mapjoin(str, self.visit_bounds, " ") + "\n"

        for t in self.connections():
            rv += t.toStr(1) + "\n"

        return rv

    def __repr__(self) -> str:
        return str(self)