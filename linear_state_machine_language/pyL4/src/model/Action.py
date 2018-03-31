from itertools import chain

from src.model.EventsAndTraces import breachActionId, breachSituationId, interveneOnDelayId
from src.independent.util import indent, castid
from src.constants_and_defined_types import ActionParamId, SituationId, ActionId, LOOP_KEYWORD, \
    LocalVarId
from src.independent.typing_imports import *
from src.model.ActionRule import PartyFutureActionRule, ActionRule
from src.model.StateTransform import StateTransform
from src.model.Statement import LocalVarDec, Statement
from src.model.Situation import Situation, ParamsDec
from src.model.Sort import Sort
from src.model.Term import Term


T = TypeVar('T')

class Action:
    def __init__(self, action_id:ActionId) -> None:
        self.action_id = action_id
        self.dest_situation_id : SituationId = castid(SituationId,"to be set after constructor")
        self.traversal_bounds: Optional[Any] = None # SExpr
        self.allowed_subjects: Optional[Any] = None # SExpr
        self.action_description: Optional[str] = None
        self.local_vars: Dict[LocalVarId, LocalVarDec] = dict()
        self.is_compound = False

        self.following_anon_situation : Optional[Situation] = None

        self.state_transform : Optional[StateTransform] = None
        self.preconditions: List[Term] = []
        self.postconditions: List[Term] = []
        self.prose_refs : List[str] = []

        self.futures : List[PartyFutureActionRule] = []

        self.param_sorts_by_name: ParamsDec = dict()  # str param id -> str sort id
        self.param_names : List[ActionParamId] = []
        self.param_name_to_ind : Dict[ActionParamId,int] = dict()

    def param_sort(self, ind_or_name:Union[str,int]) -> Sort:
        if isinstance(ind_or_name,str):
            return self.param_sorts_by_name[castid(ActionParamId,ind_or_name)]
        if isinstance(ind_or_name,int):
            return self.param_sorts_by_name[self.param_names[ind_or_name]]

    def state_transform_statements(self) -> Iterator[Statement]:
        if self.state_transform:
            for s in self.state_transform.statements:
                yield s

    def add_future_action_rule(self, far:PartyFutureActionRule) -> None:
        self.futures.append(far)

    def future_action_rules(self) -> Iterator[PartyFutureActionRule]:
        return self.futures.__iter__()

    def forEachTerm(self, f:Callable[[Term],Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        rviter : Iterable[T] = iteraccum_maybe or []
        for statement in self.state_transform_statements():
            rviter = chain(rviter,statement.forEachTerm(f,rviter))
        for rule in self.future_action_rules():
            assert isinstance(rule,ActionRule)
            rviter = chain(rviter, rule.forEachTerm(f,rviter))
        return rviter

    def forEach(self, pred:Callable[[Any],bool], f:Callable[[Any],Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        for statement in self.state_transform_statements():
            rviter = chain(rviter, statement.forEach(pred, f))
        for rule in self.future_action_rules():
            assert isinstance(rule, ActionRule)
            rviter = chain(rviter, rule.forEach(pred, f))
        return rviter
    
    def __str__(self) -> str:
        return self.toStr(0)

    def toStr(self,i:int) -> str:
        rv = indent(i) + f"action {self.action_id}"
        if self.param_sorts_by_name:
            rv += '(' + ", ".join(param + ": " + str(self.param_sorts_by_name[param]) for param in self.param_sorts_by_name) + ')'
        else:
            rv += "() " # makes it look better with python syntax highlighting
        if self.dest_situation_id != LOOP_KEYWORD:
            rv +=  f" transitions to {self.dest_situation_id}"
        else:
            rv += f" non-transitioning"
        if len(self.preconditions) > 0 or self.state_transform or len(self.postconditions) > 0:
            rv += ":\n"
        else:
            rv += "\n" # nothing more in this action decl
        for pre in self.preconditions:
            rv += indent(i+1) + "pre: " + str(pre) + "\n"
        # if self.traversal_bounds:
        #     rv += indent(1) + "prove " + mapjoin(str, self.traversal_bounds, " ") + "\n"
        if self.state_transform:
            rv += str(self.state_transform) + "\n"

        for t in self.future_action_rules():
            rv += t.toStr(i + 1) + "\n"

        for pre in self.postconditions:
            rv += indent(i+1) + "post: " + str(pre) + "\n"

        if self.following_anon_situation:
            anon_situation_str = self.following_anon_situation.toStr(i+1)
            rv += anon_situation_str

        return rv

    def __repr__(self) -> str:
        return str(self)

    # def vulnerableParties(self) -> List[RoleId]:
    #     print("BROKEN")

    @staticmethod
    def breachAction(*role_ids:str) -> 'Action':
        rv = Action(breachActionId(*role_ids))
        rv.dest_situation_id = breachSituationId(*role_ids)
        return rv

    @staticmethod
    def interveneOnDelayAction(role_id:str) -> 'Action':
        rv = Action(interveneOnDelayId(role_id))
        rv.dest_situation_id = breachSituationId(role_id)
        return rv
