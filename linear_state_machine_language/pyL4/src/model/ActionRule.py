from itertools import chain

from src.independent.util import indent, mapjoin
from src.constants_and_defined_types import *
from src.independent.typing_imports import *
from src.model.PartialEvalTerm import PartialEvalTerm
from src.model.Term import Term


class FutureActionRuleType(NamedTuple):
    rid: RoleId
    aid: ActionId
    kw: DeonticKeyword

# ABSTRACT
class ActionRule:
    def __init__(self,
                 role_id: RoleId,
                 action_id: ActionId,
                 args: Optional[List[RuleBoundActionParamId]],
                 entrance_enabled_guard: Optional[Term]) -> None:
        self.role_id = role_id
        self.action_id = action_id
        self.entrance_enabled_guard = entrance_enabled_guard
        self.time_constraint: Term
        self.where_clause: Optional[Term] = None

        self.args = args
        self.args_name_to_ind = {self.args[i]:i for i in range(len(self.args))} if self.args else None
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

    if ar.action_id == FULFILLED_SECTION_LABEL and str(ar.time_constraint) == 'immediately':
        rv += indent(indent_level) + FULFILLED_SECTION_LABEL
        return rv

    if ar.role_id == ENV_ROLE:
        rv += indent(indent_level) + ar.action_id
    else:
        rv += indent(indent_level) + f"{ar.role_id} {ar.deontic_keyword} {ar.action_id}"

    if fixed_param_vals:
        assert not ar.args
        rv += f"({mapjoin(str , fixed_param_vals, ', ')})"
    elif ar.args:
        assert not ar.fixed_args
        rv += f"({mapjoin(str , ar.args, ', ')})"
    elif ar.fixed_args:
        rv += f"({mapjoin(str , ar.fixed_args, ', ')})"

    if ar.role_id == ENV_ROLE and str(ar.time_constraint) == 'immediately':
        return rv

    if ar.time_constraint:
        rv += " " + str(ar.time_constraint)

    if ar.where_clause:
        rv += " where " + str(ar.where_clause)

    return rv

class PartyFutureActionRule(ActionRule):
    def __init__(self,
                 src_action_id: ActionId,
                 role_id: RoleId,
                 action_id: ActionId,
                 args: Optional[List[RuleBoundActionParamId]],
                 entrance_enabled_guard: Optional[Term],
                 deontic_keyword: DeonticKeyword) -> None:
        super().__init__(role_id, action_id, args, entrance_enabled_guard)
        self.src_action_id = src_action_id
        self.deontic_keyword = deontic_keyword

    def toStr(self, i:int, fixed_param_vals : Optional[List[Data]] = None) -> str:
        return common_party_action_rule_toStr(self, i, fixed_param_vals)


class PartlyInstantiatedPartyFutureActionRule(NamedTuple):
    # todo QUESTION: Should time constraint be partially evaluated??
    """
    It's derived from, and points at, a PartyFutureActionRule. Call that its parent rule.
    Its parent rule's `entrance_enabled_guard` evaluated to True when this thing was created.
    It has values for all `GlobalVarId`s that occur in its parent's `where_clause` or `time_constraint`.
    It has values for all `ActionBoundActionParamId`s that occur in its parent's `where_clause`  or `time_constraint`. Such variables can only
    occur if its parent rule is defined in a `FollowingSection` declaration, since that is the only way that
    an `ActionBoundActionParamId` can be in the scope of a `where_clause` or `time_constraint`.
    It has values for none of the `RuleBoundActionParamId`s that occur in its `where_clause` or `time_constraint`.
    """
    rule : PartyFutureActionRule
    pe_where_clause : Optional[PartialEvalTerm] # "pe" for PartialEval
    fixed_param_vals : Optional[List[Data]]
    # pe_time_constraint : PartialEvalTerm

    # not necessary because comes from .pe_where_clause.gvar_subst (== .pe_time_constraint.gvar_subst):
    #   gvar_vals : GVarSubst
    # not necessary because comes from .pe_where_clause.abap_subst (== .pe_time_constraint.abap_subst)
    #   ab_aparam_vals : List[Any] # "ab_aparam" short for action-bound action-param.
    #   aba_param_vals_dict : Dict[ActionBoundActionParamId,Any]  # shouldn't be necessary except maybe for debugging

    def __repr__(self) -> str:
        if self.pe_where_clause:
            return self.rule.toStr(0) + " with partly-instantiated where clause " + str(self.pe_where_clause)
        else:
            return self.rule.toStr(0, self.fixed_param_vals)
        # return "PartlyInstantiatedPartyFutureActionRule..."

# ABSTRACT
class NextActionRule(ActionRule):
    def __init__(self,
                 src_id: SectionId,
                 role_id: RoleId,
                 action_id: ActionId,
                 args: Optional[List[RuleBoundActionParamId]],
                 entrance_enabled_guard: Optional[Term]) -> None:
        super().__init__(role_id, action_id, args, entrance_enabled_guard)
        self.time_constraint : Term
        self.src_id = src_id

class PartyNextActionRule(NextActionRule):
    def __init__(self,
                 src_id: SectionId,
                 role_id: RoleId,
                 action_id: ActionId,
                 args: Optional[List[RuleBoundActionParamId]],
                 entrance_enabled_guard: Optional[Term],
                 deontic_keyword: DeonticKeyword) -> None:
        super().__init__(src_id, role_id, action_id, args, entrance_enabled_guard)

        self.deontic_keyword = deontic_keyword

    def toStr(self, i:int) -> str:
        return common_party_action_rule_toStr(self, i)


class EnvNextActionRule(NextActionRule):
    def __init__(self,
                 src_id: SectionId,
                 action_id: ActionId,
                 args: Optional[List[RuleBoundActionParamId]],
                 entrance_enabled_guard: Optional[Term]) -> None:
        super().__init__(src_id, ENV_ROLE, action_id, args, entrance_enabled_guard)

    def toStr(self, i:int) -> str:
        rv : str = ""
        indent_level = i
        if self.entrance_enabled_guard:
            rv = indent(indent_level) + "if " + str(self.entrance_enabled_guard) + ":\n"
            indent_level += 1

        assert self.role_id == ENV_ROLE
        rv += indent(indent_level) + self.action_id

        if self.args:
            assert not self.fixed_args
            rv += f"({mapjoin(str , self.args, ', ')})"
        elif self.fixed_args:
            rv += f"({mapjoin(str , self.fixed_args, ', ')})"

        if str(self.time_constraint) == 'immediately':
            return rv

        if self.time_constraint:
            rv += " " + str(self.time_constraint)

        return rv



