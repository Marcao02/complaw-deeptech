# from typing import Union, List, Dict, Any, Tuple
from typing import Set
from LSMEventState import *

class FormalContract:
    def __init__(self, name: str) -> None:
        self.name = name
        self.estates : Dict[EventStateId, EventState] = None
        self.params : Dict[str,Sort] = None  # paramname -> sort
        self.start_state: str = None # EventState id

    def can_transition(self, transid1, transid2) -> bool:
        if transid1 is None and transid2 == self.start_state:
            return True
        return self.estates[transid1].can_transition_to(transid2)

    def transitions(self) -> Iterator[TransitionClause]:
        for estate in self.estates.values():
            for t in estate.transitions():
                yield t

class LSMTop:
    def __init__(self, filename:str) -> None:
        self.filename = filename
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

    def estate(self, id:str):
        return self.formal_contract.estates[id]

    def varObj(self, varname:str):
        if varname in self.global_vars:
            return self.global_vars[varname]
        else:
            logging.error("Todo: see if it's a local var")