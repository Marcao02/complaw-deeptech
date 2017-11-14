# from typing import Union, List, Dict, Any, Tuple, Callable
from typing import Iterator, List,  Set, Optional, NamedTuple
from constants_and_defined_types import *
from util import indent

from LSMStatements import *

class CodeBlock:
    def __init__(self, statements: List[CodeBlockStatement]) -> None:
        self.statements = statements

    def __str__(self):
        rv = ""
        def line(s, newindent=0):
            nonlocal rv
            rv += 4*newindent*" " + str(s) + "\n"

        line("entrance:",1)
        for statement in self.statements:
            line(statement, 2)

        return rv

class TransitionClause:
    def __init__(self,
                 src_id: EventStateId,
                 dest_id: EventStateId,
                 actor_id:ActorId,
                 deontic_modality: DeonticModality,
                 enabled_guard: Optional[Term] = None) -> None:
        self.actor_id = actor_id
        self.src_id = src_id
        self.dest_id = dest_id
        self.deontic_modality = deontic_modality
        self.args : SExpr = None

        self.conditions : SExpr = None # deadline and guard
        self.where_clause : Term = None
        self.deadline_clause : Term = None
        self.enabled_guard = enabled_guard

    def __str__(self) -> str:
        rv = f"{self.actor_id} {self.deontic_modality} {self.dest_id}"
        if self.deadline_clause:
            rv += " " + str(self.deadline_clause)
        return rv


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
    def __init__(self, name: str, is_action_state: bool) -> None:
        self.name = name
        self.params : Dict[str,str] = None  # str param -> str sort
        self.prose_refs : List[str] = None
        self.code_block : CodeBlock = None
        self.local_vars: Dict[str,LocalVarDec] = dict()
        self.connections_by_role: Dict[ActorId, Set[TransitionClause]] = None
        self.description: str = None
        self.is_action_state = is_action_state

    def vulnerableParties(self) -> List[ActorId]:
        return list(self.connections_by_role.keys())

    def can_transition_to(self, transid:str) -> bool:
        return any(t.dest_id == transid for t in self.transitions())

    def transitions(self) -> Iterator[TransitionClause]:
        for actorblock in self.connections_by_role.values():
            for t in actorblock:
                yield t

        # if self.nonactor_block:
        #     for t in self.nonactor_block:
        #         yield t

    def __str__(self):
        rv = ""
        if self.is_action_state:
            rv += "action "
        else:
            rv += "env-event "
        rv += self.name
        if self.params:
            rv += f'({mapjoin(str, self.params, ", ")}) '
        rv += " with unique destination:"

        rv += "\n"
        if self.code_block:
            rv += str(self.code_block)
            rv += "\n"

        for t in self.transitions():
            rv += indent(1) + str(t)
            rv += "\n"

        return rv