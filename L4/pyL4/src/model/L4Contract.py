# from typing import Union, List, Dict, Any, Tuple
from datetime import datetime, timedelta
from itertools import chain
from typing import Type

from src.independent.FileCoord import FileCoord
from src.independent.typing_imports import *
from src.constants_and_defined_types import *
from src.model.Term import Term
from src.model.Action import Action
from src.model.ActionRule import ActionRule, PartyFutureActionRule, NextActionRule, EnvNextActionRule, \
    FutureActionRuleType, PartyNextActionRule
from src.model.BoundVar import StateVar
from src.model.ContractClaim import ContractClaim, StateInvariant
from src.model.ContractParamDec import ContractParamDec
from src.model.Definition import Definition
from src.model.StateVarDec import StateVarDec
from src.model.L4Macro import L4Macro, L4BlockMacro
from src.model.Situation import Situation
from src.model.Sort import Sort

S = TypeVar('S')
T = TypeVar('T')

class L4Contract:
    def __init__(self, filename:str) -> None:
        self.filename = filename
        self.contract_name : str = "to be set"
        self.prose_contract : Dict[ProseClauseId, str] = dict() # mapping clause id string to clause string

        self.start_situation_id = cast(SituationId, "start situation id to be assigned")

        self.state_var_decs : Dict[StateVarId, StateVarDec] = dict()
        self.contract_params : Dict[ContractParamId, ContractParamDec] = dict()

        self.claims: Iterable[ContractClaim] = []
        self.state_invariants: Iterable[StateInvariant] = []
        self.end_of_trace_claims: List[Term] = []

        self.ontology_axioms: List[Term] = []
        # temporary till process the entire thing:
        self.ontology_fns: List[str] = []


        # TODO: make `definitions` `term_definitions`
        self.definitions: Dict[DefinitionId, Definition] = dict()
        self.sort_definitions: Dict[str, Sort] = dict()
        self.expanded_sort_definitions: Dict[str, Sort] = dict()

        self.roles : List[RoleId] = [ENV_ROLE, ARBITER_ROLE]
        self.sorts : Set[Sort] = set()
        self.fnsymb_names : Set[str] = set()

        self.situations_by_id: Dict[SituationId, Situation] = dict()
        self.actions_by_id: Dict[ActionId, Action] = dict()

        self.possible_floating_rule_types: Set[FutureActionRuleType] = set()

        self.ordered_declarations : List[Union[Action,Situation]] = list()

        self.macros : Dict[str, L4Macro] = dict()
        self.blockmacros: Dict[str, L4BlockMacro] = dict()

        self.timeunit : str = "d" # default to days
        self.start_datetime: Optional[datetime] = None
        self.default_action_timelimit: Optional[timedelta] = None

        # self.flags : Optional[Set[str]] = None

        self.dot_file_name: Optional[str] = None  # for input file to graphviz
        self.img_file_name: Optional[str] = None  # for graphviz output

        self.all_sorted_names : Dict[str,Sort] = dict()
        self.write_bounds : Dict[str,Tuple[int,Optional[int]]] = dict()

        self.nlg_names : Dict[str,str] = dict()
        self.nlg_sections: List[Any] = []
        self.nlg_definitions: Dict[str,str] = dict()
        self.contract_params_nonoperative : Dict[str, str] = dict()

        self.local_vars_eliminated = False
        self.if_then_else_terms_eliminated = False
        self.must_eliminated = False


    def register_sorted_name(self,name:str,sort:Sort):
        if name in self.all_sorted_names:
            assert sort == self.all_sorted_names[name], f"Every occurrence of a sort-typed name (with the exception of event " \
                                                        f"rule variables)\nmust have the same sort.\n"\
                                                        f"This makes it easier for you interact with the system (no need " \
                                                        f"to specify a typing context),\nand easier for us to ensure correctness.\n" \
                                                        f"You've used {name} with both {sort} and {self.all_sorted_names[name]}.\n" \
                                                        f"See {self.filename}."
        else:
            self.all_sorted_names[name] = sort


    def nextaction_rules(self) -> Iterator[NextActionRule]:
        for s in self.situations_iter():
            for nar in s.action_rules():
                yield nar
    def futureaction_rules(self) -> Iterator[PartyFutureActionRule]:
        for a in self.actions_iter():
            for far in a.future_action_rules():
                yield far
    def action_rules(self) -> Iterator[ActionRule]: return chain(self.nextaction_rules(), self.futureaction_rules())

    def new_state_var_ref(self, varname, coord:Optional[FileCoord] = None) -> StateVar:
        return StateVar(self.state_var_decs[varname], coord)

    def situation_mentions_action_in_nextaction_rule(self, situationid:SituationId, actionid:ActionId) -> bool:
        cursituation = self.situation(situationid)
        for c in cursituation.action_rules():
            if isinstance(c, PartyNextActionRule) or isinstance(c, EnvNextActionRule):
                if c.action_id == actionid:
                    return True
            else:
                raise NotImplementedError

        return False

    # def actions_sometimes_available_from_situation(self, situationid:SituationId, actionid:ActionId) -> Set[ActionId]:
    #     cursituation = self.situation(situationid)
    #     rv : Set[ActionId] = set()
    #     for c in cursituation.future_action_rules():
    #         if isinstance(c, PartyNextActionRule) or isinstance(c, EnvNextActionRule):
    #             if c.action_id == actionid:
    #                 rv.add(c.action_id)
    #         else:
    #             raise NotImplementedError
    #
    #     return rv

    def transitions(self) -> Iterator[NextActionRule]:
        for sit in self.situations_iter():
            for c in sit.action_rules():
                yield c

    def situations_iter(self) -> Iterable[Situation]: return self.situations_by_id.values()
    def situation_ids(self) -> Iterable[SituationId]: return self.situations_by_id.keys()
    def situation(self, anid:SituationId) -> Situation:
        assert anid in self.situations_by_id, f"situation id '{anid}' not found"
        return self.situations_by_id[anid]
    def sources_of_action(self, a: Action) -> Iterable[Situation]:
        return filter(lambda s: self.situation_mentions_action_in_nextaction_rule(s.situation_id,a.action_id), self.situations_iter())

    def actions_iter(self) -> Iterable[Action]: return self.actions_by_id.values()
    def action_ids(self) -> Iterable[ActionId]: return self.actions_by_id.keys()
    def action(self, anid: ActionId) -> Action:
        if anid not in self.actions_by_id:
            raise SyntaxError(f"No Action found with id {anid}")
        return self.actions_by_id[anid]
        # return self.actions_by_id[anid] if anid in self.actions_by_id else None
    def actions_with_destination(self, destid:str) -> Iterable[Action]:
        return filter(lambda a: a.dest_situation_id == destid, self.actions_iter())
    def actions_nonderived_with_destination(self, destid:str) -> Iterable[Action]:
        return filter(lambda a: not a.dest_situation_id.startswith("Breach") and not a.dest_situation_id.startswith("InterveneOnDelay") and a.dest_situation_id == destid, self.actions_iter())

    def gvarDecObj(self, varname:str) -> Optional[StateVarDec]:
        if varname in self.state_var_decs:
            return self.state_var_decs[cast(StateVarId, varname)]
        else:
            return None
        # elif isinstance(sit,Situation) and varname in sit.local_vars:
        #     return sit.local_vars[varname]

    def forEachTerm(self, f:Callable[[Term],Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        rviter : Iterable[T] = iteraccum_maybe or []
        for action in self.actions_iter():
            rviter = chain(rviter, action.forEachTerm(f,rviter))
        for situation in self.situations_iter():
            rviter = chain(rviter, situation.forEachTerm(f,rviter))
        for contract_param_dec in self.contract_params.values():
            if contract_param_dec.value_expr:
                rviter = contract_param_dec.value_expr.forEachTerm(f, rviter)
        for gvardec in self.state_var_decs.values():
            if gvardec.initval:
                rviter = gvardec.initval.forEachTerm(f,rviter)
        return rviter

    def forEach(self, pred: Callable[[Any], bool], f: Callable[[Any], Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))

        for action in self.actions_iter():
            rviter = chain(rviter, action.forEach(pred,f))

        for situation in self.situations_iter():
            rviter = chain(rviter, situation.forEach(pred,f))

        for contract_param_dec in self.contract_params.values():
            rviter = chain(rviter, contract_param_dec.forEach(pred, f))

        for gvardec in self.state_var_decs.values():
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

        line('\nstate vars:')
        for gv in self.state_var_decs.values():
            line(gv,1)

        if self.claims:
            line('\nclaims:')
            for c in self.claims:
                line(c,1)

        for x in self.ordered_declarations:
            line('')
            rv += str(x)

        return rv

    # def can_transition(self, situationid1:SituationId, situationid2:SituationId) -> bool:
    #     if situationid1 is None:
    #         return situationid2 == self.start_situation
    #
    #     cursituation = self.situation(situationid1)
    #     for c in cursituation.future_action_rules():
    #         if isinstance(c, PartyNextActionRule):
    #             action = self.action(c.action_id)
    #             if action.dest_situation_id == situationid2:
    #                 return True
    #         elif isinstance(c, ActionRuleToSituation):
    #             if c.dest_id == situationid2:
    #                 return True
    #         else:
    #             raise NotImplementedError
    #
    #     return False


def derived_destination_id(action_id:ActionId) -> SituationId:
    return cast(SituationId, "After" + action_id)
def derived_trigger_id(dest_id:SituationId) -> ActionId:
    return cast(ActionId, "Enter" + dest_id)
def derived_trigger_id_to_situation_id(action_id:ActionId) -> SituationId:
    return cast(SituationId, action_id[5:])
def is_derived_destination_id(situation_id:SituationId) -> bool:
    return situation_id.startswith("After") or situation_id.startswith("Breached_")
def is_derived_trigger_id(action_id:ActionId) -> bool:
    return action_id.startswith("Enter") or action_id.startswith("Breach_") or action_id.startswith("InterveneOnDelay_")
