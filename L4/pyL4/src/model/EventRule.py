from enum import Enum
from itertools import chain

from src.independent.util import indent, todo_once, castid
from src.independent.util_for_str import mapjoin
from src.constants_and_defined_types import *
from src.independent.typing_imports import *
from src.model.Term import Term

T = TypeVar('T')

rule_param_to_ind_cache : Dict['ActorEventRule', Dict[RuleParamId,int]] = Dict[RuleParamId,int]
def rule_to_ruleparam_to_ind(er:'ActorEventRule') -> Dict[RuleParamId,int]:
    todo_once("Once Term is immutable, can cache this without using str() for hash")
    if er.ruleparam_names:
        d = {er.ruleparam_names[i]: i for i in range(len(er.ruleparam_names))}
        return d
    else:
        return dict()
    # erhash = str(er)
    # if erhash in rule_param_to_ind_cache:
    #     return rule_param_to_ind_cache[castid(RuleParamId,erhash)]
    # else:
    #     if er.ruleparam_names:
    #         d = {er.ruleparam_names[i]:i for i in range(len(er.ruleparam_names))}
    #         rule_param_to_ind_cache[erhash] = d
    #         return d
    #     else:
    #         return dict()


#
class EventRuleContext(NamedTuple):
    ruleparam_names: Optional[Tuple[RuleParamId,...]]
    action_id: ActionId
    for_deadline_event_rule: bool

    def ruleparam_to_ind(self,name:str) -> Optional[int]:
        if not self.ruleparam_names:
            return None
        else:
            for known_name_ind in range(len(self.ruleparam_names)):
                if self.ruleparam_names[known_name_ind] == name:
                    return known_name_ind
            assert False


class ActorEventRule(NamedTuple):
    src_id: SituationId
    action_id: ActionId
    entrance_enabled_guard: Optional[Term]

    ruleparam_names: Optional[Tuple[RuleParamId, ...]]
    param_setter: Optional[Tuple[Term,...]]
    where_clause: Optional[Term]
    role_ids: List[RoleId]
    deontic_keyword: DeonticKeyword
    time_constraint: Optional[Term]
    immediate: bool

    def ruleparam_to_ind(self) -> Dict[RuleParamId,int]:
        return rule_to_ruleparam_to_ind(self)

    def forEachTerm(self, f:Callable[[Term],Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        rviter : Iterable[T] = iteraccum_maybe or []
        if self.entrance_enabled_guard:
            rviter = self.entrance_enabled_guard.forEachTerm(f,rviter)
        if self.where_clause:
            rviter = self.where_clause.forEachTerm(f,rviter)
        if self.param_setter:
            for i in range(len(self.param_setter)):
                argterm = self.param_setter[i]
                rviter = argterm.forEachTerm(f, rviter)
        if self.time_constraint:
            rviter = self.time_constraint.forEachTerm(f, rviter)
        return rviter

    def forEach(self, pred: Callable[[Any], bool], f: Callable[[Any], Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        rviter = chain(rviter, f(self.action_id), f(self.src_id))
        if pred(self):
            rviter = chain(rviter, f(self))
        if self.ruleparam_names:
            for pname in self.ruleparam_names:
                rviter = chain(rviter, f(pname))
        for roleid in self.role_ids:
            rviter = chain(rviter, f(roleid))
        if self.entrance_enabled_guard:
            rviter = chain(rviter, self.entrance_enabled_guard.forEach(pred,f))
        if self.where_clause:
            rviter = chain(rviter, self.where_clause.forEach(pred,f))
        if self.param_setter:
            for i in range(len(self.param_setter)):
                argterm = self.param_setter[i]
                rviter = chain(rviter, argterm.forEach(pred,f))
        if self.time_constraint:
            rviter = chain(rviter, self.time_constraint.forEach(pred,f))
        return rviter

    def toStr(self, i:int) -> str:
        rv: str = ""
        indent_level = i
        if self.entrance_enabled_guard:
            rv = indent(indent_level) + "if " + str(self.entrance_enabled_guard) + ":\n"
            indent_level += 1

        if self.action_id == FULFILLED_SITUATION_LABEL and self.immediate:
            rv += indent(indent_level) + FULFILLED_SITUATION_LABEL
            return rv

        rv += indent(indent_level) + f"{roles_to_str(self.role_ids)} {self.deontic_keyword} {self.action_id}"

        if self.param_setter:
            assert not self.ruleparam_names
            rv += f"({mapjoin(str , self.param_setter, ', ')})"
        elif self.ruleparam_names:
            assert not self.param_setter
            rv += f"({mapjoin(str , self.ruleparam_names, ', ')})"

        if self.time_constraint:
            rv += " when " + str(self.time_constraint)

        if self.where_clause:
            rv += " where " + str(self.where_clause)

        return rv


class DeadlineEventRule(NamedTuple):
    src_id: SituationId
    action_id: ActionId
    entrance_enabled_guard: Optional[Term]

    param_setter: Optional[Tuple[Term, ...]]
    deadline_fn: Term
    trigger_type: TriggerType

    # def ruleparam_to_ind(self) -> Dict[RuleParamId,int]:
    #     return rule_to_ruleparam_to_ind(self)

    def forEachTerm(self, f:Callable[[Term],Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        rviter : Iterable[T] = iteraccum_maybe or []
        rviter = self.deadline_fn.forEachTerm(f, rviter)
        if self.entrance_enabled_guard:
            rviter = self.entrance_enabled_guard.forEachTerm(f,rviter)
        if self.param_setter:
            for i in range(len(self.param_setter)):
                argterm = self.param_setter[i]
                rviter = argterm.forEachTerm(f, rviter)
        return rviter

    def forEach(self, pred: Callable[[Any], bool], f: Callable[[Any], Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        rviter = chain(rviter, f(self.action_id), f(self.src_id))
        rviter = self.deadline_fn.forEachTerm(f, rviter)
        if pred(self):
            rviter = chain(rviter, f(self))
        if self.entrance_enabled_guard:
            rviter = chain(rviter, self.entrance_enabled_guard.forEach(pred,f))
        if self.param_setter:
            for i in range(len(self.param_setter)):
                argterm = self.param_setter[i]
                rviter = chain(rviter, argterm.forEach(pred,f))
        return rviter

    def toStr(self, i:int) -> str:
        rv : str = ""
        indent_level = i
        if self.entrance_enabled_guard:
            rv = indent(indent_level) + "if " + str(self.entrance_enabled_guard) + ":\n"
            indent_level += 1

        rv += indent(indent_level) + self.action_id

        if self.param_setter:
            rv += f"({mapjoin(str , self.param_setter, ', ')})"

        rv += " " + self.trigger_type.name + " " + str(self.deadline_fn)

        return rv

EventRule = Union[ActorEventRule,DeadlineEventRule]

def roles_to_str(roles:Sequence[str]) -> str:
    if len(roles) == 1:
        return roles[0]
    else:
        return "{" + mapjoin(str, roles, ',') + "}"