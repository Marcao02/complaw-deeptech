from typing import Any, List, Tuple, NamedTuple, Dict, Sequence, Optional, Union, NewType, Set, cast
from itertools import chain


from src.typesystem.Sorts import *
from src.util_for_sequences import nested_list_replace, nested_list_replace_mult
from typesystem.FnTypes import OverloadedFnType
from typesystem.reducers import eliminate_type_vars, print_types_map, build_graph, eliminate_unbounded_arity


def abr_arity_fntype(dom:Any, ran:Any) -> Tuple[str, Any, Any]:
    return ('aafn', dom, ran)

# simple function type
def sfntype(*tp:Any) -> Any:
    assert len(tp) >= 2
    return ('fn',) + tp

T = 'TVar'
N = 'NVar'
D = 'DVar'

def parametric(tp:Sequence[Any], substitutions:Sequence[Any] = AllAtomicSorts, var:str = T) -> Sequence[Any]:
    # print(substitutions)
    return  [ nested_list_replace(tp, var, primtype) for primtype in substitutions ]

def parametric_mult(tp:Sequence[Any], substitutions:Sequence[Dict[str,Any]]) -> Sequence[Any]:
    # print(substitutions)
    return  [ nested_list_replace_mult(tp, subst) for subst in substitutions ]

def list_parametric_mult(tps:Sequence[Sequence[Any]], substitutions:Sequence[Dict[str,Any]]) -> Sequence[Any]:
    # print(substitutions)
    return  list(chain([ nested_list_replace_mult(tp, subst) for subst in substitutions ] for tp in tps))



overloaded_types_data : Sequence[ Tuple[Sequence[str], Any] ] = (
    (('≤', '≥', '<', '>'), (
        abr_arity_fntype(Real, Bool),
        abr_arity_fntype(TimeDelta, Bool),
        abr_arity_fntype(DateTime, Bool) )),
    (('==',),                 parametric(abr_arity_fntype(T, Bool))),
    (('!=',),                 parametric(sfntype(T, T, Bool))),
    (('ifthenelse',),         parametric(sfntype(Bool, T, T, T))),
    (('min','max','+','*') ,  parametric(abr_arity_fntype(T, T), UnboundedNumericSorts + (TimeDelta,))),
    # TODO: {0},{1}, and {0,1}.
    (('*',),                  sfntype(TimeDelta, Nat, TimeDelta)),
    (('*',), list_parametric_mult( [sfntype(('Rate', N, D), D, N), sfntype(D, ('Rate', N, D), N)], (
                {'N':Real,'D':PosInt},
                {'N':PosReal, 'D': PosInt},
                {'N':NonnegReal, 'D': PosInt}
    ))),
    (('*',), parametric(abr_arity_fntype(T, T), BoundedNumericSorts )),
    (('-',),                  parametric(sfntype(T, T, T), (Int, Real, TimeDelta))),
    (('/',),                  parametric(sfntype(T, T, T), (Real, PosReal, NonnegReal))),
    # todo Rate for /
    (('not',),                sfntype(Bool, Bool)),
    (('and','or'),            sfntype(Bool, Bool, Bool)),
    (('and*','or*'),          abr_arity_fntype(Bool, Bool)),
    # (('and*','or*'),          sfntype(Bool, Bool)),
    (('floor','round','ceil'), (
        sfntype(Real, Int),
        sfntype(NonnegReal, Nat) )),
    (('ceil',), (
        sfntype(Real, Int),
        sfntype(PosReal, PosInt) )),
    (('even','odd'),          parametric(sfntype(T, Bool), (Int, Nat, PosInt))),
    # this is not right cuz first arg type can be a superset of the other arg types.
    # need to compile these away I think. can compile to a Term with a `transpiled_from` field.
    (('+=','*='),             parametric(sfntype(T, T, T), UnboundedNumericSorts))
)

unbounded_arity_fnsymbols = {'≤', '≥', '<', '>', '==', 'or*', 'and*',
                              'min','max','+','*'}

overloaded_fn_types : Dict[str,OverloadedFnType] = dict()

fntypes_map = eliminate_type_vars(overloaded_types_data)
print_types_map(fntypes_map)

standard_types_graph = build_graph()
print("\n" + str(standard_types_graph))

eliminate_unbounded_arity({'and*':{3}, '*':{2,3}, 'max':{2,3}, 'min':{2},
                           '≤':{2,3}, '<':{2,3}, '>':{2,3},
                           '≥':{2,3}, '==':{2}, '+':{2,3}}, fntypes_map)
print_types_map(fntypes_map)

