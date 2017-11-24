from typing import Optional, Dict, List
from model.GlobalStateTransform import GlobalStateTransform
from model.GlobalStateTransformStatement import LocalVarDec
from model.SExpr import SExpr
from model.Term import Term
from model.constants_and_defined_types import ActionParamId, SortId, LocalVarId, SectionId, ActionId
from model.util import mapjoin, indent




class Action:
    def __init__(self, action_id:ActionId) -> None:
        self.action_id = action_id
        self.dest_section_id : SectionId
        self.traversal_bounds: Optional[SExpr] = None
        self.allowed_subjects: Optional[SExpr] = None
        self.action_description: Optional[str] = None
        self.local_vars: Dict[LocalVarId,LocalVarDec] = dict()
        self.is_compound = False

        self.params : Optional[Dict[ActionParamId,SortId]] = None # str param -> str sort
        self.global_state_transform : Optional[GlobalStateTransform] = None
        self.preconditions: List[Term] = []
        self.postconditions: List[Term] = []
        self.prose_refs : List[str] = []

    # def vulnerableParties(self) -> List[RoleId]:
    #     print("BROKEN")
    #     return list(self.connections_by_role.keys())

    def __str__(self):
        rv = f"action {self.action_id} transitions to {self.dest_section_id}"
        if self.params:
            rv += f'({mapjoin(str, self.params, ", ")}) '
        rv += ":\n"
        for pre in self.preconditions:
            rv += indent(1) + "pre: " + str(pre) + "\n"
        # if self.traversal_bounds:
        #     rv += indent(1) + "prove " + mapjoin(str, self.traversal_bounds, " ") + "\n"
        if self.global_state_transform:
            rv += str(self.global_state_transform)
            rv += "\n"

        for pre in self.postconditions:
            rv += indent(1) + "post: " + str(pre) + "\n"
        return rv

    def __repr__(self) -> str:
        return str(self)