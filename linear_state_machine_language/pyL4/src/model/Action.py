from typing import Optional, Dict, List, Any, Iterator, Iterable


from src.model.ActionRule import PartyFutureActionRule
from src.model.GlobalStateTransform import GlobalStateTransform
from src.model.GlobalStateTransformStatement import LocalVarDec
from src.model.SExpr import SExpr
from src.model.Section import Section, ParamsDec
from src.model.Term import Term
from src.model.constants_and_defined_types import ActionParamId_BoundBy_ActionDecl, SortId, LocalVarId, SectionId, ActionId
from src.model.util import mapjoin, indent




class Action:
    def __init__(self, action_id:ActionId) -> None:
        self.action_id = action_id
        self.dest_section_id : SectionId
        self.traversal_bounds: Optional[SExpr] = None
        self.allowed_subjects: Optional[SExpr] = None
        self.action_description: Optional[str] = None
        self.local_vars: Dict[LocalVarId,LocalVarDec] = dict()
        self.is_compound = False

        self.following_anon_section : Optional[Section] = None

        self.params : ParamsDec = dict() # str param id -> str sort id
        self.global_state_transform : Optional[GlobalStateTransform] = None
        self.preconditions: List[Term] = []
        self.postconditions: List[Term] = []
        self.prose_refs : List[str] = []

        self.futures : List[PartyFutureActionRule] = []

    def add_action_rule(self, far:PartyFutureActionRule) -> None:
        self.futures.append(far)

    def action_rules(self) -> Iterator[PartyFutureActionRule]:
        return self.futures.__iter__()

    def __str__(self) -> str:
        return self.toStr(0)

    def toStr(self,i:int) -> str:
        rv = indent(i) + f"action {self.action_id}"
        if self.params:
            rv += f'({mapjoin(str, self.params, ", ")}) '
        else:
            rv += "() " # makes it look better with python syntax highlighting
        rv +=  f" transitions to {self.dest_section_id}"
        if len(self.preconditions) > 0 or self.global_state_transform or len(self.postconditions) > 0:
            rv += ":\n"
        else:
            rv += "\n" # nothing more in this action decl
        for pre in self.preconditions:
            rv += indent(i+1) + "pre: " + str(pre) + "\n"
        # if self.traversal_bounds:
        #     rv += indent(1) + "prove " + mapjoin(str, self.traversal_bounds, " ") + "\n"
        if self.global_state_transform:
            rv += str(self.global_state_transform) + "\n"

        for t in self.action_rules():
            rv += t.toStr(i + 1) + "\n"

        for pre in self.postconditions:
            rv += indent(i+1) + "post: " + str(pre) + "\n"

        if self.following_anon_section:
            anon_section_str = self.following_anon_section.toStr(i+1)
            rv += anon_section_str

        return rv

    def __repr__(self) -> str:
        return str(self)

    # def vulnerableParties(self) -> List[RoleId]:
    #     print("BROKEN")
