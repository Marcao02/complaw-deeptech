# from typing import Union, List, Dict, Any, Tuple
from typing import Set
from LSMEventState import *

class FormalContract:
    def __init__(self, name: str) -> None:
        self.name = name
        self.estates : Dict[EventStateId, EventState] = None
        self.params : Dict[str,Sort] = None  # paramname -> sort

    def can_transition(self, transid1, transid2) -> bool:
        return self.estates[transid1].can_transition_to(transid2)

    def transitions(self) -> Iterator[TransitionClause]:
        for estate in self.estates.values():
            for t in estate.transitions():
                yield t

class L4Top:
    def __init__(self) -> None:
        self.global_vars : Dict[GlobalVarId,GlobalVar] = None
        self.claims : List[ContractClaim] = None
        self.actors : List[str] = None
        self.prose_contract : Dict[ProseClauseId, str] = None  # mapping clause id string to clause string
        self.formal_contract : FormalContract = None
        self.sorts : Set[Sort] = set()
        self.dot_file_name : str = None # for input file to graphviz
        self.img_file_name: str = None  # for graphviz output

    def can_transition(self, transid1, transid2) -> bool:
        return self.formal_contract.can_transition(transid1, transid2)

    def transitions(self) -> Iterator[TransitionClause]:
        return self.formal_contract.transitions()

    def event_states(self) -> List[EventState]:
        return list(self.formal_contract.estates.values())