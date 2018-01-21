from typing import Any, List, Tuple, NamedTuple, Dict, Sequence, Optional, Union, NewType, Set, cast, Iterable

from src.typesystem.Sorts import *
from src.util_for_sequences import nested_list_replace, nested_list_replace_mult
from src.typesystem.FnTypes import OverloadedFnType, NonoverloadedFnType, SimpleFnType, ArbArityFnType
from src.typesystem.reducers import flatten_fntype_data, print_types_map, eliminate_unbounded_arity



def arb_arity_fntype(dom:Any, ran:Any) -> Tuple[str, Any, Any]:
    return ('aafn', dom, ran)

# simple function type
def sfntype(*tp:Any) -> Any:
    assert len(tp) >= 1
    return ('fn',) + tp

T = 'TVar'
N = 'NVar'
D = 'DVar'
R = 'RVar'
typevars = {T,N,D,R}

def parametric(tp:Sequence[Any], substitutions:Iterable[Any] = AllSortData, var:str = T) -> Sequence[Any]:
    # print(substitutions)
    return  [ nested_list_replace(tp, var, primtype) for primtype in substitutions ]

def parametric_mult(tp:Sequence[Any], substitutions:Iterable[Dict[str,Any]]) -> Sequence[Any]:
    # print(substitutions)
    assert all(all(v in typevars for v in subst) for subst in substitutions)
    return  [ nested_list_replace_mult(tp, subst) for subst in substitutions ]

def list_parametric_mult(tps:Iterable[Sequence[Any]], substitutions:Iterable[Dict[str,Any]]) -> Sequence[Any]:
    # print(substitutions)
    lists = [[ nested_list_replace_mult(tp, subst) for subst in substitutions ] for tp in tps]
    rv = []
    for l in lists:
        rv.extend(l)
    return rv


todo_once("Strengthen polymorphic types")



overloaded_types_data : Sequence[ Tuple[Sequence[str], Any] ] = (
    (('event_td','next_event_td','future_event_td','sectionEntrance_td','monthStartDay_td','monthEndDay_td'), sfntype(TimeDelta)),
    (('days',), (sfntype(Nat,TimeDelta), sfntype(PosInt,PosTimeDelta))),

    (('tuple',), parametric(sfntype(T,T,('Tuple',T,T)),AllAtomicSorts)),
    (('tupleGet',), parametric(sfntype(('Tuple',T,T),'{0,1}',T), AllAtomicSorts)),

    (('mapSet',), parametric(sfntype(('TDMap', T), T,'TimeDelta', ('TDMap', T)),TDMapKeySortData)),
    (('tdGEQ',), parametric(sfntype(('TDMap', T), T,'TimeDelta', 'Bool'),TDMapKeySortData)),
    (('mapDelete',), parametric(sfntype(('TDMap', T), T, ('TDMap', T)),TDMapKeySortData)),
    (('mapHas',), parametric(sfntype(('TDMap', T), T, 'Bool'),TDMapKeySortData)),
    (('nonempty','empty'), parametric(sfntype(('TDMap',T), 'Bool'), TDMapKeySortData)),

    (('≤', '≥', '<', '>'), (
        arb_arity_fntype(Real, Bool),
        arb_arity_fntype(TimeDelta, Bool),
        arb_arity_fntype(DateTime, Bool) )),
    (('==',),                 parametric(arb_arity_fntype(T, Bool))),
    (('!=',),                 parametric(sfntype(T, T, Bool))),
    (('ifthenelse',),         parametric(sfntype(Bool, T, T, T))),
    (('min','max','+','*') ,  parametric(arb_arity_fntype(T, T), UnboundedNumericSorts.union({TimeDelta}))),
    # temp hack:
    (('min','max','+','*') ,  parametric_mult(arb_arity_fntype(('Rate', N, D), ('Rate', N, D)), (
                                              {'NVar':PosReal,'DVar':PosInt},
                                            ))
     ),

    # temp hack:
    (('max',), ( sfntype(PosReal,Real,PosReal),
                 sfntype(PosInt, Int, PosInt),)),

    (('+',), ( sfntype(PosInt,Nat,PosInt), sfntype(Nat,PosInt,PosInt),
               sfntype(PosReal,NonnegReal,PosReal), sfntype(NonnegReal,PosReal,PosReal),
               )
     ),
    # TODO: {0},{1}, and {0,1}.

    (('*',),                  sfntype(TimeDelta, Nat, TimeDelta)),
    # temp hack
    (('*',), list_parametric_mult( [sfntype(('Rate', N, D), D, R), sfntype(D, ('Rate', N, D), R)], (
                {'NVar':Real,'DVar':PosReal, 'RVar':Real},
                {'NVar':PosReal, 'DVar': PosReal, 'RVar':PosReal},
                {'NVar':NonnegReal, 'DVar': PosReal, 'RVar':NonnegReal},
                {'NVar':Real,'DVar':PosInt, 'RVar':Real},
                {'NVar':PosReal, 'DVar': PosInt, 'RVar':PosReal},
                {'NVar':NonnegReal, 'DVar': PosInt, 'RVar':NonnegReal},
    ))),
    (('*',), list_parametric_mult( [sfntype(('Rate', N, D), T, ('Rate',N,D)), sfntype(T, ('Rate', N, D), ('Rate',N,D))], (
                {'NVar':PosReal,'DVar':PosInt, 'TVar':PosReal},  # temp hack
    ))),
    (('*',), parametric(arb_arity_fntype(T, T), BoundedNumericSorts)),

    (('-',),                  parametric(sfntype(T, T, T), (Int, Real, TimeDelta))),

    (('/',),                  parametric_mult(sfntype(N, D, R), (
                                {'NVar':Real, 'DVar':PosReal, 'RVar':('Rate',Real,PosReal)},
                                {'NVar':PosReal, 'DVar':PosReal, 'RVar':('Rate',PosReal,PosReal)},
                                {'NVar':NonnegReal, 'DVar':PosReal, 'RVar':('Rate',NonnegReal,PosReal)},
                                {'NVar':Real, 'DVar':PosInt, 'RVar':('Rate',Real,PosInt)},
                                {'NVar':PosReal, 'DVar':PosInt, 'RVar':('Rate',PosReal,PosInt)},
                                {'NVar':NonnegReal, 'DVar':PosInt, 'RVar':('Rate',NonnegReal,PosInt)}
                                )
                              )
     ),
     (('/',), list_parametric_mult( [sfntype(N, ('Rate', N, D), R),], (
                {'NVar':Real,'DVar':PosReal, 'RVar':Real},
                {'NVar':PosReal, 'DVar': PosReal, 'RVar':PosReal},
                {'NVar':NonnegReal, 'DVar': PosReal, 'RVar':NonnegReal},
                {'NVar':Real,'DVar':PosInt, 'RVar':Real},
                {'NVar':PosReal, 'DVar': PosInt, 'RVar':PosReal},
                {'NVar':NonnegReal, 'DVar': PosInt, 'RVar':NonnegReal},
            ))
    ),

    (('/',), (sfntype(PosTimeDelta, PosTimeDelta, PosReal),
              sfntype(TimeDelta, PosTimeDelta, NonnegReal),

              )

     ),

    (('^',), ( sfntype(PosReal,Real,PosReal),
                sfntype(PosInt,Nat,PosInt),
               )

     ),

    (('not',),                sfntype(Bool, Bool)),
    (('and','or'),            sfntype(Bool, Bool, Bool)),
    (('and*','or*'), arb_arity_fntype(Bool, Bool)),
    # (('and*','or*'),          sfntype(Bool, Bool)),
    (('floor','round','ceil'), (
        sfntype(Real, Int),
        sfntype(PosReal, Nat),
        sfntype(NonnegReal, Nat) )),
    (('ceil',), (
        sfntype(Real, Int),
        sfntype(PosReal, PosInt),
        sfntype(NonnegReal, Nat)
    )),
    (('even','odd'),          parametric(sfntype(T, Bool), (Int, Nat, PosInt))),
)

unbounded_arity_fnsymbols = {'≤', '≥', '<', '>', '==', 'or*', 'and*',
                              'min','max','+','*'}

fntype_data_map = flatten_fntype_data(overloaded_types_data)

for fntype_data in fntype_data_map.values():
    assert isinstance(fntype_data, list)
    for nonover_fntype_data in fntype_data:
        assert isinstance(nonover_fntype_data, tuple)
        for x in nonover_fntype_data:
            assert isinstance(x, str) or isinstance(x, tuple)
            if isinstance(x,str):
                assert x not in typevars, x
            if isinstance(x, tuple):
                for y in x:
                    assert  y not in typevars, str(y) + " and " + str(nonover_fntype_data)

print_types_map(fntype_data_map)

def sortdata_to_sort(data:Any) -> Sort:
    if isinstance(data,str):
        return data
    else:
        return NonatomicSort(data[0], tuple(sortdata_to_sort(x) for x in data[1:]))

def nonoverloaded_fntype_data_to_object(data:Any) -> NonoverloadedFnType:
    if data[0] == 'fn':
        return SimpleFnType(tuple(sortdata_to_sort(x) for x in data[1:]))
    else:
        assert data[0] == 'aafn' and len(data) == 3, data
        return ArbArityFnType(sortdata_to_sort(data[1]), sortdata_to_sort(data[2]))

def makeNiceFnTypeMap() -> Dict[str,OverloadedFnType]:
    fntype_map : Dict[str,OverloadedFnType] = dict()
    for f in fntype_data_map:
        fntype_map[f] = OverloadedFnType( tuple(nonoverloaded_fntype_data_to_object(datapart) for datapart in fntype_data_map[f]),
                                          dict(), set())
    return fntype_map

fntypes_map = makeNiceFnTypeMap()

