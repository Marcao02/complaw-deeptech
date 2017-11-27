import logging
from typing import Set, Union, Any, Optional

from model.SExpr import SExprOrStr
from model.constants_and_defined_types import *
from model.L4Contract import L4Contract, is_derived_destination_id, is_derived_trigger_id


# just for typing!!
class L4ContractConstructorInterface:
    top : L4Contract
    referenced_nonderived_section_ids: Set[SectionId]
    referenced_nonderived_action_ids: Set[ActionId]
    def syntaxError(self, expr: SExprOrStr, msg: Optional[str] = None):
        pass

def referenced_nonderived_section_ids_equal_defined_nonderived_section_ids(it:L4ContractConstructorInterface) -> bool:
    referenced_nonderived_section_ids = set(filter( lambda x: not is_derived_destination_id(x), it.top.section_ids())
                                            ).union([FULFILLED_SECTION_LABEL])
    itset = it.referenced_nonderived_section_ids.union([FULFILLED_SECTION_LABEL])
    if itset != set(referenced_nonderived_section_ids):
        logging.warning(
            f"""
    ISSUE: Set of referenced nonderived section ids ≠ set of declared nonderived section ids:
    Referenced : {str(sorted(itset))} 
    Defined    : {str(sorted(set(referenced_nonderived_section_ids)))}"""
        )
        return False
    else:
        return True

def referenced_nonderived_action_ids_equal_defined_nonderived_action_ids(it:L4ContractConstructorInterface) -> bool:
    referenced_nonderived_action_ids = set(filter( lambda x: not is_derived_trigger_id(x), it.top.action_ids()))
    if  it.referenced_nonderived_action_ids != set(referenced_nonderived_action_ids):
        logging.warning(
            f"""
    ISSUE: Set of referenced nonderived action ids ≠ set of declared nonderived action ids:
    Referenced : {str(sorted(it.referenced_nonderived_action_ids))} 
    Defined    : {str(sorted(set(referenced_nonderived_action_ids)))}"""
        )
        return False

    return True

def actions_correct_number_args(it:L4ContractConstructorInterface) -> bool:
    for c in it.top.connections:
        action = it.top.action(c.action_id)
        args_required = len(c.args) if c.args else 0
        args_given = len(action.params) if action.params else 0
        if args_given != args_required:
            it.syntaxError('', f"Wrong number of action parameters for {action.action_id}")
            return False
    return True

test_fns = [
    referenced_nonderived_section_ids_equal_defined_nonderived_section_ids,
    referenced_nonderived_action_ids_equal_defined_nonderived_action_ids,
    actions_correct_number_args
]
