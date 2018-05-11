from typing import Sequence, Tuple, Optional

from src.constants_and_defined_types import *
from src.independent.util import castid
from src.model.EventsAndTraces import CompleteTrace, Trace, Event, breachSituationId, EventType, breachActionId


K = 1000
M = 1000000

def event(action_id:str, role_id:str = ENV_ROLE,
          timestamp: int = 0,
          params:Optional[Dict[str, Data]] = None,
          eventType: Optional[EventType] = None ) -> Event:
    if eventType is None:
        eventType = EventType.env_next if role_id == ENV_ROLE else EventType.party_next
    params = params or dict()
    return Event(action_id=castid(ActionId, action_id), role_id=castid(RoleId, role_id),
                 timestamp= timestamp,
                 actionparam_subst=cast(ActionParamSubst, params) if params else None,
                 actionparam_subst_list=list(params.values()) if params else None,
                 type=eventType)

def foevent(action_id:str, role_id:str = ENV_ROLE,
          timestamp: int = 0, params:Optional[Dict[str, Data]] = None) -> Event:
    return event(action_id, role_id, timestamp , params, EventType.fulfill_floating_obligation)

def fpevent(action_id:str, role_id:str = ENV_ROLE,
          timestamp:int = 0, params:Optional[Dict[str, Data]] = None) -> Event:
    return event(action_id,role_id,timestamp,params,EventType.use_floating_permission)

inc_timestamp = 0

def firstTSEvent(action_id:str,
                 role_id:str = ENV_ROLE,
                 params:Optional[Dict[str, Data]] = None,
                 eventType: Optional[EventType] = None) -> Event:
    global inc_timestamp
    inc_timestamp = 0
    return nextTSEvent(action_id, role_id, params, eventType)

def nextTSEvent(action_id:str,
                role_id:str = ENV_ROLE,
                params:Optional[Dict[str, Data]] = None,
                eventType: Optional[EventType] = None) -> Event:
    global inc_timestamp
    newevent = event(action_id, role_id, inc_timestamp, params, eventType)
    inc_timestamp = inc_timestamp + 1
    return newevent

def sameTSEvent(action_id:str,
                role_id:str = ENV_ROLE,
                params:Optional[Dict[str, Data]] = None,
                eventType: Optional[EventType] = None) -> Event:
    global inc_timestamp
    return event(action_id, role_id, inc_timestamp, params, eventType)
