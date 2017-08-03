# from typing import Union, List, Set, Dict, Any, Tuple, Callable
from typing import Iterator, List
from constants_and_defined_types import *

from LSMStatements import *

class CodeBlock:
    def __init__(self, statements: List[CodeBlockStatement]) -> None:
        self.statements = statements

class TransitionClause:
    def __init__(self, src_id: EventStateId, dest_id: EventStateId, actor_id:ActorId) -> None:
        self.actor_id = actor_id
        self.src_id = src_id
        self.dest_id = dest_id
        self.args = None
        self.conditions : List[str] = None

class ActorBlock:
    def __init__(self, transitions:Dict[EventStateId,TransitionClause], actor_id:ActorId) -> None:
        self.actor_id = actor_id
        self.transitions = transitions
class NonactorBlock:
    def __init__(self, transitions:Dict[EventStateId,TransitionClause]) -> None:
        self.transitions = transitions


class EventState:
    def __init__(self, name: str) -> None:
        self.name = name
        self.params : Dict[str,str] = None  # str param -> str sort
        self.prose_refs : List[str] = None
        self.code_block : CodeBlock = None
        self.nonactor_block: NonactorBlock = None  # str event state name -> TransitionClause
        self.proper_actor_blocks: Dict[ActorId, ActorBlock] = None  # str actor name -> str event state name -> TransitionClause
        self.description: str = None

    def vulnerableParties(self) -> List[ActorId]:
        return list(self.proper_actor_blocks.keys())

    def can_transition_to(self, transid:str) -> bool:
        return any(t.dest_id == transid for t in self.transitions())

    def transitions(self) -> Iterator[TransitionClause]:
        for actorblock in self.proper_actor_blocks.values():
            for t in actorblock.transitions.values():
                yield t

        if self.nonactor_block:
            for t in self.nonactor_block.transitions.values():
                yield t
