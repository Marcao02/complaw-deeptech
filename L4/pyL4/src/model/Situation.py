from itertools import chain

from src.independent.util_for_str import mapjoin
from src.model.EventsAndTraces import breachSituationId
from src.independent.util import indent
from src.constants_and_defined_types import *
from src.independent.typing_imports import *
from src.model.ActionRule import NextActionRule, FutureActionRuleType, roles_to_str
from src.model.Term import Term

T = TypeVar('T')

class Situation:
    def __init__(self, situation_id: SituationId) -> None:
        self.situation_id = situation_id
        self.visit_bounds: Optional[Any] = None  # currently SExpr
        self.description: Optional[str] = None
        self.prose_refs: List[str] = []

        self.preconditions: List[Term] = []

        # currently can replace this with just a list...
        self._action_rules: List[NextActionRule] = []


        self.parent_action_id : Optional[ActionId] = None

        self.possible_floating_rule_types : Set[FutureActionRuleType] = set()

    def is_anon(self) -> bool:
        return self.parent_action_id is not None

    def add_action_rule(self, nar:NextActionRule) -> None:
        self._action_rules.append(nar)

    def action_rules(self) -> Iterable[NextActionRule]:
        for r in self._action_rules:
            yield r

    def forEachTerm(self, f:Callable[[Term],Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        rviter : Iterable[T] = iteraccum_maybe or []
        for rule in self.action_rules():
            rviter= chain(rviter, rule.forEachTerm(f,rviter))
        return rviter

    def forEach(self, pred: Callable[[Any], bool], f: Callable[[Any], Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        for rule in self.action_rules():
            rviter = chain(rviter, rule.forEach(pred, f))
        return rviter

    def __str__(self):
        return self.toStr(0)

    def toStr(self,i:int) -> str:
        if self.parent_action_id is None:
            rv = indent(i) + f"situation {self.situation_id}:\n"
        else:
            rv = indent(i) + f"following situation:\n"

        for pre in self.preconditions:
            rv += indent(i+1) + "pre: " + str(pre) + "\n"

        if self.description:
            rv += indent(i+1) + "description: " + self.description + "\n"

        # if self.visit_bounds:
        #     rv += indent(1) + "prove " + mapjoin(str, self.visit_bounds, " ") + "\n"
        rules = sorted(self.action_rules(), key = lambda x: roles_to_str(x.role_ids) + x.action_id )
        for t in rules:
            rv += t.toStr(i+1) + "\n"

        return rv

    def __repr__(self) -> str:
        return str(self)

    # def vulnerableParties(self) -> List[RoleId]:
    #     print("BROKEN")
    #     return list(self.action_rules_by_role.keys())
    #

    @staticmethod
    def breachSituation(*role_ids: str) -> 'Situation':
        rv = Situation(breachSituationId(*role_ids))
        return rv

