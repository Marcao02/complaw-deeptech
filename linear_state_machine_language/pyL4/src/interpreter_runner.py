from typing import Union, Optional, cast

from src.model.Literal import Literal
from src.model.util import castid
from src.sexpr_to_L4Contract import L4ContractConstructor
from src.model.constants_and_defined_types import SectionId, GVarSubst, ContractParamId
from src.interpreter import ExecEnv as ExecEnvOld
from src.interpreter_no_floating import ExecEnv as ExecEnvNF
from src.model.EventsAndTraces import Trace, CompleteTrace
from src.model.L4Contract import L4Contract


def evalTrace(it:Union[Trace,CompleteTrace], prog:L4Contract, debug=False):
    env : Union[ExecEnvOld,ExecEnvNF]
    if len(list(prog.futureaction_rules())) > 0:
        # raise Exception("ExecEnvOld works but we're trying to phase it out.")
        env = ExecEnvOld(prog)
    else:
        env = ExecEnvNF(prog)

    if isinstance(it, CompleteTrace):
        for contract_param in it.contract_param_subst:
            # replacing hardcoded contrat param vals with passed in ones
            supplied_val = it.contract_param_subst[contract_param]
            paramdec = prog.contract_params[castid(ContractParamId, contract_param)]
            # if isinstance(paramdec.value_expr,Literal):
            if isinstance(paramdec.value_expr,Literal) and supplied_val is not None:
                paramdec.value_expr.lit = supplied_val
            else:
                paramdec.value_expr = L4ContractConstructor.mk_literal(supplied_val)

        return env.evalTrace(trace = it.events,
                             finalSectionId = cast(SectionId, it.final_section),
                             final_var_vals = cast(Optional[GVarSubst], it.final_values),
                             verbose=True, debug=debug)
    else:
        return env.evalTrace(trace = it, verbose=True, debug=debug)

