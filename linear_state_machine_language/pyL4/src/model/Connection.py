from typing import Optional, NamedTuple

from model.constants_and_defined_types import *
from model.statements import Term
from model.Action import ActionId
from model.SExpr import SExpr

class Connection(NamedTuple):
    src_id: SectionId
    role_id: RoleId
    action_id: ActionId
    args: Optional[SExpr]
    deontic_modality: DeonticModality
    deadline_clause: Term
    enabled_guard: Optional[Term]

    def __str__(self) -> str:
        rv : str
        if self.action_id == FULFILLED_SECTION_LABEL and str(self.deadline_clause) == 'immediately':
            return FULFILLED_SECTION_LABEL
        # if self.action_id == FULFILLED_SECTION_LABEL:
        #     print(self.deadline_clause)

        if self.role_id == ENV_ROLE:
            rv = self.action_id
        else:
            rv = f"{self.role_id} {self.deontic_modality} {self.action_id}"

        if self.args:
            rv += f"({mapjoin(str , self.args, ', ')})"

        if self.role_id == ENV_ROLE and str(self.deadline_clause) == 'immediately':
            return rv

        if self.deadline_clause:
            rv += " " + str(self.deadline_clause)
        return rv