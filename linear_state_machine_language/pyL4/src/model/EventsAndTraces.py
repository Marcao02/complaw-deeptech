from typing import NamedTuple, Optional, Any, Dict, Sequence

from src.model.ActionRule import PartyFutureActionRule
from src.model.constants_and_defined_types import ActionId, RoleId, TimeStamp, ActionParamSubst


class Event(NamedTuple):
    action_id: ActionId
    role_id: RoleId
    timestamp: TimeStamp
    params: Optional[ActionParamSubst]

def breachSectionId(*role_ids:str):
    return "breach_" + "_".join(role_ids)


Trace = Sequence[Event]
class CompleteTrace(NamedTuple):
    contract_param_subst: Dict[str, Any]
    events: Trace
    final_section: str  # will need to be a SectionId
    final_values: Optional[Dict[str,Any]] = None

