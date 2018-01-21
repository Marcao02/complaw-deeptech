from typing import List, Sequence, Iterable, Any, Set, Dict, Tuple, Union, cast

from src.model.Sort import Sort, NonatomicSort
from src.model.FnTypes import OverloadedFnType, NonoverloadedFnType, SimpleFnType, ArbArityFnType

from src.util_for_sequences import nested_list_replace, nested_list_replace_mult
from src.typesystem.standard_sorts import AllAtomicSorts, TDMapKeySortData, TimeDelta, Bool, UnboundedNumericSorts, \
    PosInt, \
    PosReal, Int, NonnegReal, AllSortData, Nat, PosTimeDelta, BoundedNumericSorts, DateTime, Real, AllNumericSorts, \
    all_sort_copies_by_orig


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
j = 'jvar'

TypeData = Sequence[Any]

Natj = ('Dup',Nat,j)
PosIntj = ('Dup',PosInt,j)
def dNat(var:str) -> Tuple[str,...]:
    return ('Dup',Nat,var)

def parametric_one_var(tp_or_tps:Union[TypeData,Set[TypeData]], substitutions:Iterable[Any] = AllSortData, var:str = T) -> Sequence[Any]:
    tps : Set[TypeData]
    if isinstance(tp_or_tps, Set):
        tps = tp_or_tps
    else:
        tps = {tp_or_tps}
    rv = []
    for tp in tps:
        rv.extend( [ nested_list_replace(tp, var, primtype) for primtype in substitutions ] )
    return rv

def parametric_mult_vars(tp_or_tps:Union[TypeData,Set[TypeData]], substitutions:Iterable[Dict[str, Any]]) -> Sequence[Any]:
    assert all(all(v in typevars for v in subst) for subst in substitutions)
    tps: Set[TypeData]
    if isinstance(tp_or_tps, Set):
        tps = tp_or_tps
    else:
        tps = {tp_or_tps}
    rv = []
    for tp in tps:
        rv.extend( [ nested_list_replace_mult(tp, subst) for subst in substitutions ] )
    return rv

def dupmult(tp_or_tps:Union[TypeData,Set[TypeData]], substitutions:Iterable[Dict[str, str]]) -> Sequence[Any]:
    tps: Set[TypeData]
    if isinstance(tp_or_tps, Set):
        tps = tp_or_tps
    else:
        tps = {tp_or_tps}
    rv = []
    for tp in tps:
        rv.extend([nested_list_replace_mult(tp, subst) for subst in substitutions])

"""
Algo to work on a recursive tuple that may contain some terms of the form ('Dup',sort,dupvar). 
First, figure out which such terms it contains. Returns a Set[Tuple[Sort,dupvar:str]] 
Let G be that set.
For each (sort,dupvar) in G:
    For each dup in dups_used[sort]
        
 



"""




overloaded_types_data : Sequence[ Tuple[Sequence[str], Any] ] = (
    (('event_td','next_event_td','future_event_td','sectionEntrance_td','monthStartDay_td','monthEndDay_td'), sfntype(TimeDelta)),
    (('days',), (sfntype(Nat,TimeDelta), sfntype(PosInt,PosTimeDelta))),

    (('tuple',), parametric_one_var(sfntype(T, T, ('Tuple', T, T)), AllAtomicSorts)),
    (('tupleGet',), parametric_one_var(sfntype(('Tuple', T, T), '{0,1}', T), AllAtomicSorts)),

    (('mapSet',), parametric_one_var(sfntype(('TDMap', T), T, 'TimeDelta', ('TDMap', T)), TDMapKeySortData)),
    (('tdGEQ',), parametric_one_var(sfntype(('TDMap', T), T, 'TimeDelta', 'Bool'), TDMapKeySortData)),
    (('mapDelete',), parametric_one_var(sfntype(('TDMap', T), T, ('TDMap', T)), TDMapKeySortData)),
    (('mapHas',), parametric_one_var(sfntype(('TDMap', T), T, 'Bool'), TDMapKeySortData)),
    (('nonempty','empty'), parametric_one_var(sfntype(('TDMap', T), 'Bool'), TDMapKeySortData)),

    (('≤', '≥', '<', '>'), (
        arb_arity_fntype(Real, Bool),
        arb_arity_fntype(PosReal, Bool),
        arb_arity_fntype(TimeDelta, Bool),
        arb_arity_fntype(DateTime, Bool) )),
    (('==',), parametric_one_var(arb_arity_fntype(T, Bool))),
    (('!=',), parametric_one_var(sfntype(T, T, Bool))),
    (('ifthenelse',), parametric_one_var(sfntype(Bool, T, T, T))),
    (('min','max','+','*') , parametric_one_var(arb_arity_fntype(T, T), UnboundedNumericSorts.union({TimeDelta}).union(BoundedNumericSorts))),
    # temp hack:
    (('min','max','+','*') , parametric_mult_vars(arb_arity_fntype(('Rate', N, D), ('Rate', N, D)), (
                                              {'NVar':PosReal,'DVar':PosInt},
                                            ))
     ),

    # temp hack:
    (('max',), ( sfntype(PosReal,Real,PosReal),
                 sfntype(PosInt, Int, PosInt),)),

    (('+',), ( sfntype(PosIntj,Natj,PosIntj), sfntype(Natj,PosIntj,PosIntj),
                sfntype(PosInt,Nat,PosInt), sfntype(Nat,PosInt,PosInt),
               sfntype(PosReal,NonnegReal,PosReal), sfntype(NonnegReal,PosReal,PosReal),
               )
     ),
    # TODO: {0},{1}, and {0,1}.

    (('*',),                  sfntype(TimeDelta, Nat, TimeDelta)),
    # temp hack
    (('*',), parametric_mult_vars( {sfntype(('Rate', N, D), D, R), sfntype(D, ('Rate', N, D), R)}, (
                {'NVar':Real,'DVar':PosReal, 'RVar':Real},
                {'NVar':PosReal, 'DVar': PosReal, 'RVar':PosReal},
                {'NVar':NonnegReal, 'DVar': PosReal, 'RVar':NonnegReal},
                {'NVar':Real,'DVar':PosInt, 'RVar':Real},
                {'NVar':PosReal, 'DVar': PosInt, 'RVar':PosReal},
                {'NVar':NonnegReal, 'DVar': PosInt, 'RVar':NonnegReal},
    ))),
    (('*',), parametric_mult_vars( {sfntype(('Rate', N, D), T, ('Rate',N,D)), sfntype(T, ('Rate', N, D), ('Rate',N,D))}, (
                {'NVar':PosReal,'DVar':PosInt, 'TVar':PosReal},  # temp hack
    ))),
    # (('*',), parametric_one_var(arb_arity_fntype(T, T), BoundedNumericSorts)),

    (('-',), parametric_one_var(sfntype(T, T, T), (Int, Real, TimeDelta))),

    (('/',), parametric_mult_vars(sfntype(N, D, R), (
                                {'NVar':Real, 'DVar':PosReal, 'RVar':('Rate',Real,PosReal)},
                                {'NVar':PosReal, 'DVar':PosReal, 'RVar':('Rate',PosReal,PosReal)},
                                {'NVar':NonnegReal, 'DVar':PosReal, 'RVar':('Rate',NonnegReal,PosReal)},
                                {'NVar':Real, 'DVar':PosInt, 'RVar':('Rate',Real,PosInt)},
                                {'NVar':PosReal, 'DVar':PosInt, 'RVar':('Rate',PosReal,PosInt)},
                                {'NVar':NonnegReal, 'DVar':PosInt, 'RVar':('Rate',NonnegReal,PosInt)}
                                )
                                  )
     ),
     (('/',), parametric_mult_vars( sfntype(N, ('Rate', N, D), R), (
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
    (('even','odd'), parametric_one_var(sfntype(T, Bool), (Int, Nat, PosInt))),
)

unbounded_arity_fnsymbols = {'≤', '≥', '<', '>', '==', 'or*', 'and*',
                              'min','max','+','*'}


def flatten_fntype_data(_overloaded_types_data:Sequence[ Tuple[Sequence[str], Any]]) -> Dict[str, List[Sequence[Any]]]:
    fntypes_map_ : Dict[str, List[Sequence[Any]]] = dict()
    for pair in _overloaded_types_data:
        fst = pair[0]
        snd : Any = pair[1]
        symbs : Sequence[str] = cast(Sequence[str], (fst,) if isinstance(fst,str) else fst)
        print("?",pair)
        fntypes = cast(List[Sequence[Any]], [snd] if (snd[0] == 'fn' or snd[0] == 'aafn') else list(snd))
        for symb in symbs:
            if symb not in fntypes_map_:
                fntypes_map_[symb] = list(fntypes)
            else:
                fntypes_map_[symb] = fntypes_map_[symb] + fntypes

    return fntypes_map_


def eliminate_unbounded_arity(arity_occurrences: Dict[str,Set[int]], fntypes_map: Dict[str, List[Sequence[Any]]]) -> None:
    for f in fntypes_map:
        # if len(arity_occurrences[f]) == 0:
        #     continue

        ftypes = fntypes_map[f]
        for i in range(len(ftypes)-1,-1,-1):
            ftype = ftypes[i]
            if ftype[0] != 'aafn':
                continue
            assert len(ftype) == 3
            del ftypes[i]
            if f in arity_occurrences:
                dom = ftype[1]
                ran = ftype[2]
                for arity in arity_occurrences[f]:
                    ftypes.append(('fn',) + (dom,)*arity + (ran,))

# def temp_normalize_sorts(prog:L4Contract) -> None:
#     for eliminated_sort in TEMP_SORT_IDENTIFICATION:
#         prog.sorts.remove(eliminated_sort)
#
#     def temp_normalize_sort(s: Sort) -> Sort:
#         if s in TEMP_SORT_IDENTIFICATION:
#             return TEMP_SORT_IDENTIFICATION[s]
#         else:
#             return s

def print_types_map(fntypes_map:Dict[str, List[Sequence[Any]]]):
    for symb in fntypes_map:
        print(symb)
        print("\t", str(fntypes_map[symb]))
    print(sum(len(fntypes_map[f]) for f in fntypes_map), "simple function types total.")


def check_type_vars_gone():
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
        fntype_map[f] = OverloadedFnType( [nonoverloaded_fntype_data_to_object(datapart) for datapart in fntype_data_map[f]],
                                          dict(), set())
    return fntype_map

# """
# depends on dups_used. modifies first arg.
# """
# def subst_concrete_dups(fntypes: Dict[str,OverloadedFnType]):
#     newparts : List[NonoverloadedFnType] = []
#     subst : Dict[Tuple[str,str], NonatomicSort] = dict()
#     for fnsymb,oft in fntypes.items():
#         newparts.clear()
#         for noft in oft.parts:
#             subst.clear()
#             for sort in noft.parts:
#                 if isinstance(sort,NonatomicSort):
#                     sortop = sort.sortop
#                     if sortop == 'Dup':
#                         dupsort = cast(Sort,sort.args[0])
#                         dupvar = cast(str,sort.args[1])


# def isSimpleNumericFnType(ft:NonoverloadedFnType) -> bool:
#     if isinstance(ft,ArbArityFnType):
#         return ft.dom in AllNumericSorts and ft.dom == ft.ran
#     if isinstance(ft,SimpleFnType):
#         solesort = ft.ran
#         return solesort in AllNumericSorts and all(ft.dom[i] == solesort for i in range(len(ft.dom)))

# def copy_sft_if_allowed(ft:NonoverloadedFnType) -> Iterable[NonoverloadedFnType]:
#     if not isSimpleNumericFnType(ft):
#         return []
#     solesort = ft.ran
#     if solesort not in all_sort_copies_by_orig:
#         return []
#
#     ft_copies : List[NonoverloadedFnType] = []
#     # for candidate_supersort in all_sort_copies_by_orig:
#     #     if standard_types_graph.hasEdge(solesort, candidate_supersort):
#     for sort1 in all_sort_copies_by_orig[solesort]:
#         if isinstance(ft, ArbArityFnType):
#             ft_copies.append( ArbArityFnType(sort1,sort1) )
#         if isinstance(ft, SimpleFnType):
#             ft_copies.append( SimpleFnType(tuple(sort1 for inst in ft_copies)) )
#     return ft_copies



fntype_data_map = flatten_fntype_data(overloaded_types_data)
print_types_map(fntype_data_map)
fntypes_map : Dict[str,OverloadedFnType] = makeNiceFnTypeMap()


    # for fnsymb,oft in fntypes_map.items():
#     new_nofts : List[NonoverloadedFnType] = []
#     for noft in oft.parts:
#         print(noft)
#         new_nofts.extend(copy_sft_if_allowed(noft))
#     if len(new_nofts) > 0:
#         print("some new ones")
#         oft.parts.extend(new_nofts)