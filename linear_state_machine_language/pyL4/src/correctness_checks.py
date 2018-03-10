import logging
from itertools import chain
from typing import Optional, Iterable

from src.model.FnTypes import OverloadedFnType
from src.independent.SExpr import SExprOrStr
from src.constants_and_defined_types import *
from src.model.ActionRule import ActionRule
from src.model.L4Contract import L4Contract, is_derived_destination_id, is_derived_trigger_id


class L4ContractConstructorInterface:
    top : L4Contract
    referenced_nonderived_situation_ids: Set[SituationId]
    referenced_nonderived_action_ids: Set[ActionId]
    def syntaxError(self, expr: SExprOrStr, msg: Optional[str] = None):
        raise Exception(f"{msg}\nExpression: {expr}")

def referenced_nonderived_situation_ids_equal_defined_nonderived_situation_ids(it:L4ContractConstructorInterface) -> bool:
    defined_nonderived_situation_ids = set(filter( lambda x: not is_derived_destination_id(x), it.top.situation_ids())
                                            ).union([FULFILLED_SITUATION_LABEL])
    itset = it.referenced_nonderived_situation_ids.union([FULFILLED_SITUATION_LABEL])
    if itset != set(defined_nonderived_situation_ids):
        logging.warning(
            f"""
    ISSUE: Set of referenced nonderived situation ids ≠ set of declared nonderived situation ids:
    Referenced : {str(sorted(itset))} 
    Defined    : {str(sorted(set(defined_nonderived_situation_ids)))}"""
        )
        return False
    else:
        return True

def referenced_nonderived_action_ids_equal_defined_nonderived_action_ids(it:L4ContractConstructorInterface) -> bool:
    defined_nonderived_action_ids = set(filter( lambda x: not is_derived_trigger_id(x), it.top.action_ids()))
    if  it.referenced_nonderived_action_ids != set(defined_nonderived_action_ids):
        logging.warning(
            f"""
    ISSUE: Set of referenced nonderived action ids ≠ set of declared nonderived action ids:
    Referenced : {str(sorted(it.referenced_nonderived_action_ids))} 
    Defined    : {str(sorted(set(defined_nonderived_action_ids)))}"""
        )
        return False

    return True

def actions_correct_number_args(it:L4ContractConstructorInterface) -> bool:
    for c in it.top.nextaction_rules():
        action = it.top.action(c.action_id)
        args_required = len(c.arg_vars_bound_by_rule) if c.arg_vars_bound_by_rule else (len(c.fixed_args) if c.fixed_args else 0)
        args_given = len(action.param_sorts_by_name) if action.param_sorts_by_name else 0
        if args_given != args_required:
            it.syntaxError('', f"Wrong number of action parameters for {action.action_id}")
            return False
    return True

def role_ids_recognized(it:L4ContractConstructorInterface) -> bool:
    for rule in chain(cast(Iterable[ActionRule], it.top.nextaction_rules()), cast(Iterable[ActionRule],it.top.futureaction_rules())):
        role_id = rule.role_id
        if role_id not in it.top.roles:
            it.syntaxError('', f"Don't recongize role id {role_id}")
            return False

    return False

test_fns = [
    referenced_nonderived_situation_ids_equal_defined_nonderived_situation_ids,
    referenced_nonderived_action_ids_equal_defined_nonderived_action_ids,
    actions_correct_number_args,
    role_ids_recognized
]
