# from typing import Union, List, Dict, Any, Tuple, Callable
from typing import Iterator, List,  Set, Optional, NamedTuple
from constants_and_defined_types import *

from LSMStatements import *

class CodeBlock:
    def __init__(self, statements: List[CodeBlockStatement]) -> None:
        self.statements = statements

class TransitionClause:
    def __init__(self, src_id: EventStateId, dest_id: EventStateId,
                 actor_id:ActorId,
                 deontic_modality: DeonticModality, deontic_guard: Optional[Term] = None) -> None:
        self.actor_id = actor_id
        self.src_id = src_id
        self.dest_id = dest_id
        self.deontic_modality = deontic_modality
        self.args : SExpr = None

        self.conditions : SExpr = None # deadline and guard
        self.where_clause : Term = None
        self.deadline_clause: SExpr = None

# class TransitionClause(NamedTuple):
#     actor_id : str
#     src_id : str
#     dest_id : str
#     deontic_modality : Optional[str]
#     args: SExpr
#     conditions: SExpr # deadline and guard
#     where_clause: Optional[Term]
#     deadline_clause: Optional[SExpr]
#
# y = TransitionClause('a', 'b', 's', None, [], [], None, None)
# x = TransitionClause(actor_id = 'a', src_id = 'b', dest_id = 's', deontic_modality = None, args = [], conditions = [], where_clause = None, deadline_clause = None)


class EventState:
    def __init__(self, name: str) -> None:
        self.name = name
        self.params : Dict[str,str] = None  # str param -> str sort
        self.prose_refs : List[str] = None
        self.code_block : CodeBlock = None
        self.local_vars: Dict[str,LocalVarDec] = dict()
        # self.nonactor_block: NonactorBlock = None  # str event state name -> TransitionClause
        # self.proper_actor_blocks: Dict[ActorId, ActorBlock] = None  # effectively (str actor name) -> (str event state name) -> TransitionClause
        self.nonactor_block: Set[TransitionClause] = None
        self.proper_actor_blocks: Dict[ActorId, Set[TransitionClause]] = None
        self.description: str = None

    def vulnerableParties(self) -> List[ActorId]:
        return list(self.proper_actor_blocks.keys())

    def can_transition_to(self, transid:str) -> bool:
        return any(t.dest_id == transid for t in self.transitions())

    def transitions(self) -> Iterator[TransitionClause]:
        for actorblock in self.proper_actor_blocks.values():
            for t in actorblock:
                yield t

        if self.nonactor_block:
            for t in self.nonactor_block:
                yield t

    def __str__(self):
        return self.name