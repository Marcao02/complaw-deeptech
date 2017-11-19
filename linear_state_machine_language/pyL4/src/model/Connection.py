from enum import Enum
from typing import Optional, NamedTuple, Union

from model.Term import Term
from model.constants_and_defined_types import *
from model.SExpr import SExpr
from model.util import indent, mapjoin

class ConnectionType(Enum):
    # toSection = 1 # not used anymore
    toAction = 2
    toEnvAction = 3

class ConnectionToAction(NamedTuple):
    src_id: SectionId
    role_id: RoleId
    action_id: ActionId
    args: Optional[SExpr]
    deadline_clause: Term
    enabled_guard: Optional[Term]

    deontic_modality: DeonticModality

    def toStr(self, i:int) -> str:
        rv : str = ""
        indent_level = i
        if self.enabled_guard:
            rv = indent(indent_level) + "if " + str(self.enabled_guard) + ":\n"
            indent_level += 1

        if self.action_id == FULFILLED_SECTION_LABEL and str(self.deadline_clause) == 'immediately':
            rv += indent(indent_level) + FULFILLED_SECTION_LABEL
            return rv

        if self.role_id == ENV_ROLE:
            rv += indent(indent_level) + self.action_id
        else:
            rv += indent(indent_level) + f"{self.role_id} {self.deontic_modality} {self.action_id}"

        if self.args:
            rv += f"({mapjoin(str , self.args, ', ')})"

        if self.role_id == ENV_ROLE and str(self.deadline_clause) == 'immediately':
            return rv

        if self.deadline_clause:
            rv += " " + str(self.deadline_clause)
        return rv


class ConnectionToEnvAction(NamedTuple):
    # NOT YET IMPLEMENTED
    src_id: SectionId
    role_id: RoleId
    action_id: ActionId
    args: Optional[SExpr]
    deadline_clause: Term
    enabled_guard: Optional[Term]

    def toStr(self, i:int) -> str:
        rv : str = ""
        indent_level = i
        if self.enabled_guard:
            rv = indent(indent_level) + "if " + str(self.enabled_guard) + ":\n"
            indent_level += 1

        assert self.role_id == ENV_ROLE
        rv += indent(indent_level) + self.action_id

        if self.args:
            rv += f"({mapjoin(str , self.args, ', ')})"

        if str(self.deadline_clause) == 'immediately':
            return rv

        if self.deadline_clause:
            rv += " " + str(self.deadline_clause)

        return rv

Connection = Union[ConnectionToAction, ConnectionToEnvAction]