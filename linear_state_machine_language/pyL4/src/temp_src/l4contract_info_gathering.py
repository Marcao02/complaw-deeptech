from src.model.BoundVar import ActionBoundActionParam, StateTransformLocalVar, GlobalVar, ContractParam, RuleBoundActionParam
from src.model.Literal import StringLit, RoleIdLit, DeadlineLit, SimpleTimeDeltaLit, BoolLit, FloatLit, IntLit, Literal
from src.model.Term import Term, FnApp
from src.model.L4Contract import L4Contract
from src.model.Sort import Sort
from src.independent.typing_imports import *

def what_sorts_used(prog:L4Contract) -> Iterable[Sort]:
    def f(t:Term):
        if isinstance(t, Literal):
            if isinstance(t, IntLit):
                if t.lit == 0:
                    yield "{0}"
                elif t.lit == 1:
                    yield "{1}"
                elif t.lit > 0:
                    yield "PosInt"
                yield "Int"
            elif isinstance(t, FloatLit):
                if t.lit == 0:
                    yield "{0}"
                elif t.lit == 1:
                    yield "{1}"
                elif 0 < t.lit < 1:
                    yield "(0,1)"
                elif t.lit > 1:
                    yield "PosReal"
                yield "Real"
            elif isinstance(t, BoolLit):
                yield "Bool"
            elif isinstance(t, SimpleTimeDeltaLit):
                if t.num > 0:
                    yield "PosTimeDelta"
                else:
                    yield "TimeDelta"
            elif isinstance(t, DeadlineLit):
                yield "Bool"
            elif isinstance(t, RoleIdLit):
                yield "RoleId"
            elif isinstance(t, StringLit):
                yield "String"
        elif isinstance(t, (StateTransformLocalVar, GlobalVar)):
            yield t.vardec.sort
        elif isinstance(t, ActionBoundActionParam):
            yield t.action.param_sorts_by_name[t.name]
        elif isinstance(t, ContractParam):
            yield t.paramdec.sort
        elif isinstance(t, RuleBoundActionParam):
            action = prog.action(t.action_rule.action_id)
            yield action.param_sort(t.ind)

    return prog.forEachTerm(f)


def what_fnsymbols_used(prog:L4Contract) -> Iterable[str]:
    def f(t:Term):
        if isinstance(t, FnApp):
            yield t.fnsymb_name


    return prog.forEachTerm(f)

def what_fnsymbols_used2(prog:L4Contract) -> Iterable[str]:
    pred = lambda t: isinstance(t,FnApp)
    def f(t:FnApp):
        # if isinstance(t, FnApp):
        yield t.fnsymb_name

    return prog.forEach(pred,f)