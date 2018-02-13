# from typing import Union, List, Dict, Any, Tuple
from itertools import chain
from typing import Type

from src.independent.FileCoord import FileCoord
from src.independent.typing_imports import *
from src.constants_and_defined_types import *
from src.model.Term import Term
from src.model.Action import Action
from src.model.ActionRule import ActionRule, PartyFutureActionRule, NextActionRule, EnvNextActionRule, \
    FutureActionRuleType, PartyNextActionRule
from src.model.BoundVar import GlobalVar
from src.model.ContractClaim import ContractClaim, StateInvariant
from src.model.ContractParamDec import ContractParamDec
from src.model.Definition import Definition
from src.model.StateVarDec import StateVarDec
from src.model.L4Macro import L4Macro
from src.model.Section import Section
from src.model.Sort import Sort

S = TypeVar('S')

class L4Contract:
    def __init__(self, filename:str) -> None:
        self.filename = filename
        self.contract_name : str = "to be set"
        self.prose_contract : Dict[ProseClauseId, str] = dict() # mapping clause id string to clause string

        self.start_section_id = cast(SectionId, "start section id to be assigned")

        self.global_var_decs : Dict[StateVarId, StateVarDec] = dict()
        self.claims : Iterable[ContractClaim] = []
        self.state_invariants : Iterable[StateInvariant] = []
        self.contract_params : Dict[ContractParamId, ContractParamDec] = dict()

        # TODO: make `definitions` `term_definitions`
        self.definitions: Dict[DefinitionId, Definition] = dict()
        self.sort_definitions: Dict[str, Sort] = dict()
        self.expanded_sort_definitions: Dict[str, Sort] = dict()

        self.roles : List[RoleId] = [ENV_ROLE]
        self.sorts : Set[Sort] = set()
        self.fnsymb_names : Set[str] = set()

        self.sections_by_id: Dict[SectionId, Section] = dict()
        self.actions_by_id: Dict[ActionId, Action] = dict()

        self.possible_floating_rule_types: Set[FutureActionRuleType] = set()

        self.ordered_declarations : List[Union[Action,Section]] = list()

        self.macros : Dict[str, L4Macro] = dict()

        self.timeunit : str = "none given"

        self.dot_file_name: Optional[str] = None  # for input file to graphviz
        self.img_file_name: Optional[str] = None  # for graphviz output

    def nextaction_rules(self) -> Iterator[NextActionRule]:
        for s in self.sections_iter():
            for nar in s.action_rules():
                yield nar
    def futureaction_rules(self) -> Iterator[PartyFutureActionRule]:
        for a in self.actions_iter():
            for far in a.future_action_rules():
                yield far
    def action_rules(self) -> Iterator[ActionRule]: return chain(self.nextaction_rules(), self.futureaction_rules())

    def new_global_var_ref(self, varname, coord:Optional[FileCoord] = None) -> GlobalVar:
        return GlobalVar(self.global_var_decs[varname], coord)

    def section_mentions_action_in_nextaction_rule(self, sectionid:SectionId, actionid:ActionId) -> bool:
        cursection = self.section(sectionid)
        for c in cursection.action_rules():
            if isinstance(c, PartyNextActionRule) or isinstance(c, EnvNextActionRule):
                if c.action_id == actionid:
                    return True
            else:
                raise NotImplementedError

        return False

    # def actions_sometimes_available_from_section(self, sectionid:SectionId, actionid:ActionId) -> Set[ActionId]:
    #     cursection = self.section(sectionid)
    #     rv : Set[ActionId] = set()
    #     for c in cursection.future_action_rules():
    #         if isinstance(c, PartyNextActionRule) or isinstance(c, EnvNextActionRule):
    #             if c.action_id == actionid:
    #                 rv.add(c.action_id)
    #         else:
    #             raise NotImplementedError
    #
    #     return rv

    def transitions(self) -> Iterator[NextActionRule]:
        for sec in self.sections_iter():
            for c in sec.action_rules():
                yield c

    def sections_iter(self) -> Iterable[Section]: return self.sections_by_id.values()
    def section_ids(self) -> Iterable[SectionId]: return self.sections_by_id.keys()
    def section(self, anid:SectionId) -> Section:
        assert anid in self.sections_by_id, f"section id '{anid}' not found"
        return self.sections_by_id[anid]
    
    def actions_iter(self) -> Iterable[Action]: return self.actions_by_id.values()
    def action_ids(self) -> Iterable[ActionId]: return self.actions_by_id.keys()
    def action(self, anid: ActionId) -> Action:
        if anid not in self.actions_by_id:
            raise SyntaxError(f"No Action found with id {anid}")
        return self.actions_by_id[anid]
        # return self.actions_by_id[anid] if anid in self.actions_by_id else None

    def gvarDecObj(self, varname:str) -> Optional[StateVarDec]:
        if varname in self.global_var_decs:
            return self.global_var_decs[cast(StateVarId, varname)]
        else:
            return None
        # elif isinstance(sec,Section) and varname in sec.local_vars:
        #     return sec.local_vars[varname]

    def forEachTerm(self, f:Callable[[Term],Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        rviter : Iterable[T] = iteraccum_maybe or []
        for action in self.actions_iter():
            rviter = chain(rviter, action.forEachTerm(f,rviter))
        for section in self.sections_iter():
            rviter = chain(rviter, section.forEachTerm(f,rviter))
        for contract_param_dec in self.contract_params.values():
            rviter = contract_param_dec.value_expr.forEachTerm(f, rviter)
        for gvardec in self.global_var_decs.values():
            if gvardec.initval:
                rviter = gvardec.initval.forEachTerm(f,rviter)
        return rviter

    def forEach(self, pred: Callable[[Any], bool], f: Callable[[Any], Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))

        for action in self.actions_iter():
            rviter = chain(rviter, action.forEach(pred,f))

        for section in self.sections_iter():
            rviter = chain(rviter, section.forEach(pred,f))

        for contract_param_dec in self.contract_params.values():
            if contract_param_dec.name == 'LEASE_DURATION':
                print("looking at dec of LEASE_DURATION")
            rviter = chain(rviter, contract_param_dec.forEach(pred, f))

        for gvardec in self.global_var_decs.values():
            rviter = chain(rviter, gvardec.forEach(pred,f))
        return rviter

    def __str__(self) -> str:
        rv = ''
        def line(thing:Any,tabs:int=0) -> None:
            nonlocal rv
            rv += (tabs * '    ') + str(thing) + "\n"

        titleline = "file: " + self.filename
        line(len(titleline)*'-')
        line(titleline)

        line('\nroles: ' + ', '.join(self.roles))

        if len(self.contract_params) > 0:
            line("\ncontract params:")
            for cp in self.contract_params.values():
                line(cp, 1)

        line('\nglobal vars:')
        for gv in self.global_var_decs.values():
            line(gv,1)

        if self.claims:
            line('\nclaims:')
            for c in self.claims:
                line(c,1)

        for x in self.ordered_declarations:
            line('')
            rv += str(x)

        return rv

    # def can_transition(self, sectionid1:SectionId, sectionid2:SectionId) -> bool:
    #     if sectionid1 is None:
    #         return sectionid2 == self.start_section
    #
    #     cursection = self.section(sectionid1)
    #     for c in cursection.future_action_rules():
    #         if isinstance(c, PartyNextActionRule):
    #             action = self.action(c.action_id)
    #             if action.dest_section_id == sectionid2:
    #                 return True
    #         elif isinstance(c, ActionRuleToSection):
    #             if c.dest_id == sectionid2:
    #                 return True
    #         else:
    #             raise NotImplementedError
    #
    #     return False


def derived_destination_id(action_id:ActionId) -> SectionId:
    return cast(SectionId, "After" + action_id)
def derived_trigger_id(dest_id:SectionId) -> ActionId:
    return cast(ActionId, "Enter" + dest_id)
def derived_trigger_id_to_section_id(action_id:ActionId) -> SectionId:
    return cast(SectionId, action_id[5:])
def is_derived_destination_id(section_id:SectionId) -> bool:
    return section_id.startswith("After") or section_id.startswith("Breach_")
def is_derived_trigger_id(action_id:ActionId) -> bool:
    return action_id.startswith("Enter")
