# from typing import Union, List, Dict, Any, Tuple
from typing import Set, NamedTuple
from model.Section import *
from model.Action import Action
from model.Connection import *
from model.ActionWithDestination import ActionWithDestination

class L4Contract:
    def __init__(self, filename:str) -> None:
        self.filename = filename
        self.contract_name = "to be set"
        self.dot_file_name : Optional[str] = None # for input file to graphviz
        self.img_file_name: Optional[str] = None  # for graphviz output
        self.prose_contract : Dict[ProseClauseId, str] = dict() # mapping clause id string to clause string

        self.start_section: SectionId

        self.global_var_decs : Dict[GlobalVarId, GlobalVarDec] = dict()
        self.claims : List[ContractClaim] = []
        self.actors : List[str] = []
        self.contract_params : Dict[str, ContractParamDec] = dict()
        self.sorts : Set[Sort] = set()

        self.sections_by_id: Dict[SectionId, Section] = dict()
        self.actions_by_id: Dict[ActionId, Action] = dict()
        self.actionDestPair_by_id: Dict[ActionId, ActionWithDestination] = dict()
        self.connections: Set[Connection] = set()

        self.ordered_declarations : List[Union[Action,Section,ActionWithDestination]] = list()

    # def can_transition(self, transid1, transid2) -> bool:
    #     return self.formal_contract.can_transition(transid1, transid2)
    #
    # def connections(self) -> Iterator[Connection]:
    #     return self.formal_contract.connections()

    def sections_iter(self) -> Iterator[Section]:
        return self.sections_by_id.values()
    def section_ids(self) -> Iterator[str]:
        return self.sections_by_id.keys()

    def section(self, id:str):
        return self.sections_by_id[id]

    def varDecObj(self, varname:str, es:Section = None) -> Optional[Union[GlobalVarDec, LocalVarDec]]:
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

        for x in self.ordered_declarations:
            line('\n')
            rv += str(x)


        return rv