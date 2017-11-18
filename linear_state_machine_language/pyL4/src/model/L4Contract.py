# from typing import Union, List, Dict, Any, Tuple
from typing import Set, NamedTuple, Iterable, Any
from model.Section import *
from model.Action import Action
from model.Connection import *
from model.GlobalStateTransform import GlobalStateTransform



class L4Contract:
    def __init__(self, filename:str) -> None:
        self.filename = filename
        self.contract_name : str = "to be set"
        self.dot_file_name : Optional[str] = None # for input file to graphviz
        self.img_file_name: Optional[str] = None  # for graphviz output
        self.prose_contract : Dict[ProseClauseId, str] = dict() # mapping clause id string to clause string

        self.start_section: SectionId = "to be assigned"

        self.global_var_decs : Dict[GlobalVarId, GlobalVarDec] = dict()
        self.claims : List[ContractClaim] = []
        self.roles : List[str] = []
        self.contract_params : Dict[str, ContractParamDec] = dict()
        self.sorts : Set[Sort] = set()

        self.sections_by_id: Dict[SectionId, Section] = dict()
        self.actions_by_id: Dict[ActionId, Action] = dict()
        self.connections: List[Connection] = list()

        self.ordered_declarations : List[Union[Action,Section]] = list()

        self.max_section_id_len = 0
        self.max_action_id_len = 0

    #
    # def connections(self) -> Iterator[Connection]:
    #     return self.construct_main_part.connections()

    def action_sometimes_available_from_section(self, sectionid:SectionId, actionid:ActionId) -> bool:
        cursection = self.section(sectionid)
        for c in cursection.connections():
            if isinstance(c, ConnectionToAction) or isinstance(c, ConnectionToEnvAction):
                if c.action_id == actionid:
                    return True
            else:
                raise NotImplementedError

        return False


    # def can_transition(self, sectionid1:SectionId, sectionid2:SectionId) -> bool:
    #     if sectionid1 is None:
    #         return sectionid2 == self.start_section
    #
    #     cursection = self.section(sectionid1)
    #     for c in cursection.connections():
    #         if isinstance(c, ConnectionToAction):
    #             action = self.action(c.action_id)
    #             if action.dest_section_id == sectionid2:
    #                 return True
    #         elif isinstance(c, ConnectionToSection):
    #             if c.dest_id == sectionid2:
    #                 return True
    #         else:
    #             raise NotImplementedError
    #
    #     return False

    def transitions(self) -> Iterator[Connection]:
        for sec in self.sections_iter():
            for c in sec.connections():
                yield c

    def sections_iter(self) -> Iterable[Section]:
        return self.sections_by_id.values()
    def section_ids(self) -> Iterable[SectionId]:
        return self.sections_by_id.keys()
    def section(self, anid:SectionId) -> Optional[Section]:
        return self.sections_by_id[anid] if anid in self.sections_by_id else None
    
    def actions_iter(self) -> Iterable[Action]:
        return self.actions_by_id.values()
    def action_ids(self) -> Iterable[ActionId]:
        return self.actions_by_id.keys()
    def action(self, anid: ActionId) -> Optional[Action]:
        return self.actions_by_id[anid] if anid in self.actions_by_id else None

    def varDecObj(self, varname:str, sec:Section = None) -> Optional[Union[GlobalVarDec, LocalVarDec]]:
        if varname in self.global_var_decs:
            return self.global_var_decs[varname]
        # elif varname in sec.local_vars:
        #     return sec.local_vars[varname]
        else:
            return None

    def __str__(self) -> str:
        rv = ''
        def line(thing:Any,tabs:int=0) -> None:
            nonlocal rv
            rv += (tabs * '    ') + str(thing) + "\n"

        titleline = "file: " + self.filename
        line(len(titleline)*'-')
        line(titleline)

        if len(self.contract_params) > 0:
            line("\ncontract params:")
            for cp in self.contract_params.values():
                line(cp, 1)

        line('\nglobal vars:')
        for gv in self.global_var_decs.values():
            line(gv,1)

        if self.claims:
            line('\nClaims:')
            for c in self.claims:
                line(c,1)

        for x in self.ordered_declarations:
            line('')
            rv += str(x)


        return rv


def derived_destination_id(action_id:str) -> SectionId:
    return "After" + action_id
def derived_trigger_id(dest_id:str) -> ActionId:
    return "Enter" + dest_id
def is_derived_destination_id(action_id:str) -> bool:
    return action_id.startswith("After")
def is_derived_trigger_id(dest_id:str) -> bool:
    return dest_id.startswith("Enter")
def derived_trigger_id_to_section_id(action_id:str) -> SectionId:
    return action_id[5:]