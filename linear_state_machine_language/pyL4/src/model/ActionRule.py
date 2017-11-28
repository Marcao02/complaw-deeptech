from typing import Optional, List

from model.Term import Term
from model.constants_and_defined_types import *
from model.util import indent, mapjoin


class ActionRule:
    def __init__(self,
                 role_id: RoleId,
                 action_id: ActionId,
                 args: Optional[List[ActionParamId_BoundBy_ActionRule]],
                 entrance_enabled_guard: Optional[Term]) -> None:
        self.role_id = role_id
        self.action_id = action_id
        self.args = args
        self.entrance_enabled_guard = entrance_enabled_guard
        self.deadline_clause: Term
        self.where_clause: Optional[Term] = None

    def toStr(self, i:int) -> str:
        raise NotImplemented

    def __str__(self) -> str:
        return self.toStr(0)

class FuturePartyActionRule(ActionRule):
    def __init__(self,
                 src_action_id: ActionId,
                 role_id: RoleId,
                 action_id: ActionId,
                 args: Optional[List[ActionParamId_BoundBy_ActionRule]],
                 entrance_enabled_guard: Optional[Term],
                 deontic_keyword: DeonticKeyword) -> None:
        super().__init__(role_id, action_id, args, entrance_enabled_guard)
        self.src_action_id = src_action_id
        self.deontic_keyword = deontic_keyword

class NextActionRule(ActionRule):
    def __init__(self,
                 src_id: SectionId,
                 role_id: RoleId,
                 action_id: ActionId,
                 args: Optional[List[ActionParamId_BoundBy_ActionRule]],
                 entrance_enabled_guard: Optional[Term]) -> None:
        super().__init__(role_id, action_id, args, entrance_enabled_guard)
        self.src_id = src_id


class PartyNextActionRule(NextActionRule):
    def __init__(self,
                 src_id: SectionId,
                 role_id: RoleId,
                 action_id: ActionId,
                 args: Optional[List[ActionParamId_BoundBy_ActionRule]],
                 entrance_enabled_guard: Optional[Term],
                 deontic_keyword: DeonticKeyword) -> None:
        super().__init__(src_id, role_id, action_id, args, entrance_enabled_guard)

        self.deontic_keyword = deontic_keyword

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
            rv += indent(indent_level) + f"{self.role_id} {self.deontic_keyword} {self.action_id}"

        if self.args:
            rv += f"({mapjoin(str , self.args, ', ')})"

        if self.role_id == ENV_ROLE and str(self.deadline_clause) == 'immediately':
            return rv

        if self.deadline_clause:
            rv += " " + str(self.deadline_clause)

        if self.where_clause:
            rv += " where " + str(self.where_clause)

        return rv



class EnvNextActionRule(NextActionRule):
    def __init__(self,
                 src_id: SectionId,
                 action_id: ActionId,
                 args: Optional[List[ActionParamId_BoundBy_ActionRule]],
                 entrance_enabled_guard: Optional[Term]) -> None:
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
