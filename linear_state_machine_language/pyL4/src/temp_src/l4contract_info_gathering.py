from itertools import chain

from src.model.FnTypes import SimpleFnType
from src.independent.util_for_sequences import gather_to_map_to_sets
from src.model.BoundVar import ActionBoundActionParam, StateTransformLocalVar, GlobalVar, ContractParam, RuleBoundActionParam
from src.model.Literal import StringLit, RoleIdLit, DeadlineLit, SimpleTimeDeltaLit, BoolLit, FloatLit, IntLit, Literal
from src.model.Term import Term, FnApp
from src.model.L4Contract import L4Contract
from src.model.Sort import Sort
from src.independent.typing_imports import *
from src.typechecking.standard_function_types import FnTypesMap
from src.typechecking.standard_subtype_graph import SubsortGraph
# from src.typechecking.typecheck import TypeChecker


def what_sorts_used_explicitly(prog:L4Contract) -> Iterable[Sort]:
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

def what_fnsymbol_arity_pairs_used(prog:L4Contract) -> Iterable[Tuple[str,int]]:
    pred = lambda t: isinstance(t,FnApp)
    def f(t:FnApp):
        # if isinstance(t, FnApp):
        yield (t.fnsymb_name, len(t.args))
    return prog.forEach(pred,f)



# Current form no longer relevant after eliminating ArbArityFnType. Might revisit later.
# """
# For each simple fn type T of fn symbol f, include T in f's filtered overloaded type iff f is used at arity arity(T)
# in prog.
# For each arbitrary arity fn type D* -> R of fn symbol f, if f is used at arity k in prog, include D^k -> R in f's
# overloaded fn type.
# """
# def filter_fn_types_by_arity_and_remove_arbitrary_arity_fn_types(
#         prog:L4Contract,
#         fntypes:FnTypesMap) -> FilteredFnTypesMap:
#     startsorts = set(what_sorts_used_explicitly(prog))
#     used_arities = gather_to_map_to_sets( what_fnsymbol_arity_pairs_used(prog) )
#     # we're gonna modify it
#     rv : FilteredFnTypesMap = {f: FilteredOverloadedFnType(set(),dict()) for f in fntypes}
#
#     for f in fntypes:
#         if f not in used_arities:
#             # this function symbol isn't used anywhere in prog
#             del fntypes[f]
#         else:
#             # non-overloaded fn type
#             for noft in fntypes[f].parts:
#                 if isinstance(noft,SimpleFnType):
#                     if len(noft.dom) in used_arities[f]:
#                         rv[f].parts.add(noft)
#                 else: # it's an ArbArityFnType
#                     for k in used_arities[f]:
#                         rv[f].parts.add(SimpleFnType((noft.dom,)*k + (noft.ran,)))
#
#     return rv




# def filter_sorts_by_filtered_fn_types(
#         prog:L4Contract,
#         graph:SubsortGraph,
#         fntypes:FilteredFnTypesMap) -> Set[Sort]:
#     tc = TypeChecker(prog)
#     sfts : Set[SimpleFnType] = set()
#     sfts.update( *cast(Iterable[SimpleFnType],fntypes.values()) ) # type:ignore
#     for sft in sfts:
#         assert isinstance(sft,SimpleFnType)
#         pass
#     raise NotImplementedError


