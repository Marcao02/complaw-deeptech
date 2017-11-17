from model.GlobalStateTransform import GlobalStateTransform
from model.statements import *

class Action:
    def __init__(self, action_id: str, dest_section_id: str) -> None:
        self.action_id = action_id
        self.dest_section_id = dest_section_id

        self.params : Optional[Dict[str,str]] = None # str param -> str sort
        self.global_state_transform : Optional[GlobalStateTransform] = None

        self.action_description: Optional[str] = None

        self.local_vars: Dict[str,LocalVarDec] = dict()

        self.is_compound = False

        self.prose_refs : List[str] = []

    # def vulnerableParties(self) -> List[RoleId]:
    #     print("BROKEN")
    #     return list(self.connections_by_role.keys())

    def __str__(self):
        rv = f"action {self.action_id}"
        if self.params:
            rv += f'({mapjoin(str, self.params, ", ")}) '
        rv += ":\n"
        if self.global_state_transform:
            rv += str(self.global_state_transform)
            rv += "\n"
        return rv