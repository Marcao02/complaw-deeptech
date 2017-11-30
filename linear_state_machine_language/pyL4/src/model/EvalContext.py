
# todo: make subdirectory for interpreter and break up interpreter.py

# there is only one `ExecEnv`, but there are multiple `EvalContext`s
from typing import NamedTuple, Optional

from src.model.constants_and_defined_types import GVarSubst, RBAPSubst, ABAPSubst


class EvalContext(NamedTuple):
    gvarvals: GVarSubst
    abapvals: Optional[ABAPSubst]
    use_current_event_params_for_rulebound_action_params: bool

    def copy(self) -> 'EvalContext':
        assert False
        return self