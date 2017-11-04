# from typing import Union, List, Dict, Any, Tuple
from typing import Set, NamedTuple
from LSMEventState import *

class FormalContract(NamedTuple):
    name : str
    estates : Dict[EventStateId, EventState]
    start_state: str         # EventState id

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
        self.global_var_decs : Dict[GlobalVarId, GlobalVarDec] = dict()
        self.claims : List[ContractClaim] = []
        self.actors : List[str] = []
        self.contract_params : Dict[str, ContractParamDec] = dict()
        self.prose_contract : Dict[ProseClauseId, str] = dict() # mapping clause id string to clause string
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

    def varDecObj(self, varname:str, es:EventState = None) -> Optional[Union[GlobalVarDec, LocalVarDec]]:
        if varname in self.global_var_decs:
            return self.global_var_decs[varname]
        elif varname in es.local_vars:
            return es.local_vars[varname]
        else:
            return None

    def __str__(self) -> str:
        rv = ''
        def line(thing:Any,tabs:int=0) -> None:
            nonlocal rv
            rv += (tabs * '    ') + str(thing) + "\n"

        titleline = "File: " + self.filename
        line(len(titleline)*'-')
        line(titleline)

        if len(self.contract_params) > 0:
            line("\nContractParams:")
            for cp in self.contract_params.values():
                line(cp, 1)


        line('\nGlobalVars:')
        for gv in self.global_var_decs.values():
            line(gv,1)

        if self.claims:
            line('\nClaims:')
            for c in self.claims:
                line(c,1)



        return rv
