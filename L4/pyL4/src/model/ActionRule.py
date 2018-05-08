from itertools import chain

from src.independent.util import indent
from src.independent.util_for_str import mapjoin
from src.constants_and_defined_types import *
from src.independent.typing_imports import *
from src.model.Term import Term

T = TypeVar('T')

class FutureActionRuleType(NamedTuple):
    rid: RoleId
    aid: ActionId
    kw: DeonticKeyword

# ABSTRACT
class ActionRule:
    def __init__(self,
                 role_ids: List[RoleId],
                 action_id: ActionId,
                 ruleparam_names: Optional[List[RuleParamId]],
                 entrance_enabled_guard: Optional[Term]) -> None:
        self.role_ids = role_ids
        self.action_id = action_id
        self.entrance_enabled_guard = entrance_enabled_guard
        self.time_constraint: Optional[Term] = None
        self.where_clause: Optional[Term] = None

        self.ruleparam_names = ruleparam_names
        self.ruleparam_to_ind : Optional[Dict[str, int]] = \
            {self.ruleparam_names[i]:i for i in range(len(self.ruleparam_names))} \
            if self.ruleparam_names else None
        self.fixed_args: Optional[List[Term]] = None

    def forEachTerm(self, f:Callable[[Term],Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        rviter : Iterable[T] = iteraccum_maybe or []
        if self.entrance_enabled_guard:
            rviter = self.entrance_enabled_guard.forEachTerm(f,rviter)
        if self.where_clause:
            rviter = self.where_clause.forEachTerm(f,rviter)
        if self.fixed_args:
            for i in range(len(self.fixed_args)):
                argterm = self.fixed_args[i]
                rviter = argterm.forEachTerm(f, rviter)
                # rviter = chain(rviter, f(argterm))
        if self.time_constraint:
            rviter = self.time_constraint.forEachTerm(f, rviter)
            # rviter = chain(rviter, f(self.time_constraint))
        return rviter

    def forEach(self, pred: Callable[[Any], bool], f: Callable[[Any], Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        if self.entrance_enabled_guard:
            rviter = chain(rviter, self.entrance_enabled_guard.forEach(pred,f))
        if self.where_clause:
            rviter = chain(rviter, self.where_clause.forEach(pred,f))
        if self.fixed_args:
            for i in range(len(self.fixed_args)):
                argterm = self.fixed_args[i]
                rviter = chain(rviter, argterm.forEach(pred,f))
                # rviter = chain(rviter, f(argterm))
        if self.time_constraint:
            rviter = chain(rviter, self.time_constraint.forEach(pred,f))
            # rviter = chain(rviter, f(self.time_constraint))
        return rviter

    def toStr(self, i:int) -> str:
        raise NotImplemented

    def action_object(self, prog:'L4Contract') -> 'Action': #type:ignore
        return prog.action(self.action_id) # type:ignore

    def rule_varname_to_sort(self, prog:'L4Contract', name) -> 'Sort': #type:ignore
        return self.action_object(prog).param_sort(self.ruleparam_to_ind[name]) #type:ignore

    def __str__(self) -> str:
        return self.toStr(0)

    def __repr__(self) -> str:
        return self.toStr(0)
    
def common_party_action_rule_toStr(ar:Union['PartyFutureActionRule', 'PartyNextActionRule'], i:int, fixed_param_vals : Optional[List[Data]] = None) -> str:
    rv: str = ""
    indent_level = i
    if ar.entrance_enabled_guard:
        rv = indent(indent_level) + "if " + str(ar.entrance_enabled_guard) + ":\n"
        indent_level += 1

    assert str(ar.time_constraint) != 'immediately'

    if ar.action_id == FULFILLED_SITUATION_LABEL and str(ar.time_constraint) == 'immediately':
        rv += indent(indent_level) + FULFILLED_SITUATION_LABEL
        return rv


    if ar.role_ids[0] == ENV_ROLE:
        rv += indent(indent_level) + ar.action_id
    else:
        rv += indent(indent_level) + f"{roles_to_str(ar.role_ids)} {ar.deontic_keyword} {ar.action_id}"

    if fixed_param_vals:
        assert not ar.ruleparam_names
        rv += f"({mapjoin(str , fixed_param_vals, ', ')})"
    elif ar.ruleparam_names:
        assert not ar.fixed_args
        rv += f"({mapjoin(str , ar.ruleparam_names, ', ')})"
    elif ar.fixed_args:
        rv += f"({mapjoin(str , ar.fixed_args, ', ')})"

    # if ar.role_id == ENV_ROLE and str(ar.time_constraint) == 'immediately':
    #     return rv
    #
    if ar.time_constraint:
        rv += " " + str(ar.time_constraint)

    if ar.where_clause:
        rv += " where " + str(ar.where_clause)

    return rv

class PartyFutureActionRule(ActionRule):
    def __init__(self,
                 src_action_id: ActionId,
                 role_ids: List[RoleId],
                 action_id: ActionId,
                 ruleparam_names: Optional[List[RuleParamId]],
                 entrance_enabled_guard: Optional[Term],
                 deontic_keyword: DeonticKeyword) -> None:
        super().__init__(role_ids, action_id, ruleparam_names, entrance_enabled_guard)
        self.role_id = role_ids[0]
        self.src_action_id = src_action_id
        self.deontic_keyword = deontic_keyword

    def toStr(self, i:int, fixed_param_vals : Optional[List[Data]] = None) -> str:
        return common_party_action_rule_toStr(self, i, fixed_param_vals)


# ABSTRACT
class NextActionRule(ActionRule):
    def __init__(self,
                 src_id: SituationId,
                 role_ids: List[RoleId],
                 action_id: ActionId,
                 ruleparam_names: Optional[List[RuleParamId]],
                 entrance_enabled_guard: Optional[Term]) -> None:
        super().__init__(role_ids, action_id, ruleparam_names, entrance_enabled_guard)
        self.time_constraint : Optional[Term]
        self.src_id = src_id

class PartyNextActionRule(NextActionRule):
    def __init__(self,
                 src_id: SituationId,
                 role_ids: List[RoleId],
                 action_id: ActionId,
                 ruleparam_names: Optional[List[RuleParamId]],
                 entrance_enabled_guard: Optional[Term],
                 deontic_keyword: DeonticKeyword) -> None:
        super().__init__(src_id, role_ids, action_id, ruleparam_names, entrance_enabled_guard)

        self.deontic_keyword = deontic_keyword

    def toStr(self, i:int) -> str:
        return common_party_action_rule_toStr(self, i)


class EnvNextActionRule(NextActionRule):
    def __init__(self,
                 src_id: SituationId,
                 action_id: ActionId,
                 ruleparam_names: Optional[List[RuleParamId]],
                 entrance_enabled_guard: Optional[Term]) -> None:
        super().__init__(src_id, [ENV_ROLE], action_id, ruleparam_names, entrance_enabled_guard)
        self.role_id = ENV_ROLE

    def toStr(self, i:int) -> str:
        rv : str = ""
        indent_level = i
        if self.entrance_enabled_guard:
            rv = indent(indent_level) + "if " + str(self.entrance_enabled_guard) + ":\n"
            indent_level += 1

        assert len(self.role_ids) == 1 and self.role_ids[0] == ENV_ROLE
        rv += indent(indent_level) + self.action_id

        if self.ruleparam_names:
            assert not self.fixed_args
            rv += f"({mapjoin(str , self.ruleparam_names, ', ')})"
        elif self.fixed_args:
            rv += f"({mapjoin(str , self.fixed_args, ', ')})"

        # if str(self.time_constraint) == 'immediately':
        #     return rv

        if self.time_constraint:
            rv += " " + str(self.time_constraint)

        return rv



def roles_to_str(roles:Sequence[str]) -> str:
    if len(roles) == 1:
        return roles[0]
    else:
        return "{" + mapjoin(str, roles, ',') + "}"