from typing import Optional, List

from model.Term import Term
from model.constants_and_defined_types import *
from model.util import indent, mapjoin

class Connection:
    def __init__(self, src_id: SectionId, role_id: RoleId, action_id: ActionId,
                        args: Optional[List[ConnectionActionParamId]], entrance_enabled_guard: Optional[Term]) -> None:
        self.src_id = src_id
        self.role_id = role_id
        self.action_id = action_id
        self.args = args
        self.entrance_enabled_guard = entrance_enabled_guard

        self.deadline_clause: Term
        self.where_clause: Optional[Term] = None

    def toStr(self, i:int) -> str:
        raise NotImplemented


class ConnectionToAction(Connection):
    def __init__(self, src_id: SectionId, role_id: RoleId, action_id: ActionId,
                        args: Optional[List[ConnectionActionParamId]], entrance_enabled_guard: Optional[Term],
                        deontic_modality: DeonticModality) -> None:
        super().__init__(src_id, role_id, action_id, args, entrance_enabled_guard)

        self.deontic_modality = deontic_modality

    def toStr(self, i:int) -> str:
        rv : str = ""
        indent_level = i
        if self.entrance_enabled_guard:
            rv = indent(indent_level) + "if " + str(self.entrance_enabled_guard) + ":\n"
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


class ConnectionToEnvAction(Connection):
    def __init__(self, src_id: SectionId, action_id: ActionId,
                        args: Optional[List[ConnectionActionParamId]], entrance_enabled_guard: Optional[Term]) -> None:
        super().__init__(src_id, ENV_ROLE, action_id, args, entrance_enabled_guard)

    def toStr(self, i:int) -> str:
        rv : str = ""
        indent_level = i
        if self.entrance_enabled_guard:
            rv = indent(indent_level) + "if " + str(self.entrance_enabled_guard) + ":\n"
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
