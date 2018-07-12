from typing import Union, Optional, cast

from src.independent.util import castid
from src.parse_to_model.sexpr_to_L4Contract import L4ContractConstructor
from src.constants_and_defined_types import SituationId, GVarSubst, ContractParamId
from src.interpreter.interpreter import ExecEnv as ExecEnvNF
from src.model.EventsAndTraces import Trace, CompleteTrace
from src.model.L4Contract import L4Contract
from src.model.Literal import Literal


def evalTrace(it:Union[Trace,CompleteTrace], prog:L4Contract, verbose:bool=True, debug:bool=False):
    env = ExecEnvNF(prog)
    if isinstance(it, CompleteTrace):
        for contract_param in it.contract_param_subst:
            # replacing hardcoded contrat param vals with passed in ones
            supplied_val = it.contract_param_subst[contract_param]
            paramdec = prog.contract_params[castid(ContractParamId, contract_param)]
            assert supplied_val is not None
            if isinstance(paramdec.value_expr,Literal):
                paramdec.value_expr.lit = supplied_val
            else:
                paramdec.value_expr = L4ContractConstructor.mk_literal(supplied_val)
        return env.evalTrace(trace = it.events,
                             finalSituationId = cast(SituationId, it.final_situation),
                             final_var_vals = cast(Optional[GVarSubst], it.final_values),
                             verbose=verbose, debug=debug)
    else:
        return env.evalTrace(trace = it, verbose=verbose, debug=debug)

