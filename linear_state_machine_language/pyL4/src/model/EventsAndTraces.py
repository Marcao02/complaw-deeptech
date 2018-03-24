from enum import Enum
from typing import NamedTuple, Optional, Any, Dict, Sequence

from src.constants_and_defined_types import ActionId, RoleId, ABAPNamedSubst, \
    AParamsSubst


class EventType(Enum):
    use_floating_permission = 'use_floating_permission'
    fulfill_floating_obligation = 'fulfill_floating_obligation'
    party_next = 'party_next'
    env_next = 'env_next'

    def __repr__(self) -> str:
        return self.name

class Event(NamedTuple):
    action_id: ActionId
    role_id: RoleId
    timestamp: int
    params_by_abap_name: Optional[ABAPNamedSubst]
    params: Optional[AParamsSubst]
    type: EventType

def breachSituationId(*role_ids:str):
    return "Breached_" + "_".join(role_ids)
def breachActionId(*role_ids:str):
    return "Breach_" + "_".join(role_ids)
def interveneOnDelayId(role_id:str):
    return "InterveneOnDelay_" + role_id

Trace = Sequence[Event]
class CompleteTrace(NamedTuple):
    contract_param_subst: Dict[str, Any]
    events: Trace
    final_situation: str  # will need to be a SituationId
    final_values: Optional[Dict[str,Any]] = None

