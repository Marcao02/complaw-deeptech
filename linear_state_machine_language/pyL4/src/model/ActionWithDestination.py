from model.Action import Action
from model.Section import Section
from typing import List, Union, Optional, NamedTuple
from model.util import indent

def derived_destination_id(action_id:str):
    return "After" + action_id

def derived_trigger_id(dest_id:str):
    return "Enter" + dest_id

class ActionWithDestination(NamedTuple):
    action: Action
    section: Section
    # when True this compound action-section declaration gets its name from self.action.action_id
    # when False gets its name from self.section.section_id
    is_action_type_compound: bool

    def __str__(self):
        # rv = f"action with destination {self.action.action_id}"
        if self.is_action_type_compound:
            rv = f"action {self.action.action_id}"
        else:
            rv = f"section {self.section.section_id}"

        if self.action.params:
            rv += f'({mapjoin(str, self.action.params, ", ")}) '
        rv += ":\n"

        if self.action.action_description:
            rv += indent(1) + "description: " + self.action.action_description

        if self.action.global_state_transform:
            rv += str(self.action.global_state_transform)

        did_heading = False
        for t in self.section.connections():
            if not did_heading:
                rv += indent(1) + "next:\n"
                did_heading = True
            rv += t.toStr(2) + "\n"

        return rv