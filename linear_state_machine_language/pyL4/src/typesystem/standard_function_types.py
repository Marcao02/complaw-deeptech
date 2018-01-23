from itertools import chain

import time

from copy import copy

from src.independent.typing_imports import *

from src.model.Sort import Sort, NonatomicSort, AtomicSort
from src.model.FnTypes import OverloadedFnType, NonoverloadedFnType, SimpleFnType, ArbArityFnType, substarb

from src.util_for_sequences import nested_list_replace, nested_list_replace_mult
from src.typesystem.standard_sorts import AllAtomicSorts, TDMapKeySortData, TimeDelta, Bool, UnboundedNumericSorts, \
    PosInt, PosReal, Int, NonnegReal, AllSortData, Nat, PosTimeDelta, BoundedNumericSorts, DateTime, Real, AllNumericSorts, \
    all_sort_copies_by_orig, SApp

"""
We have a rich (and getting richer) numeric hierarchy in the standard library, and we have a few ways of combining
the numeric sorts, such as a Tuple operator (which is currently just a Pair operator), and a Ratio operator.
Let's call this L4's Standard Math Universe (SMU). We'll have some other things like enum sorts, but those are not important
for the current topic.

We allow alternate universes obtained from copying parts of the SMU, and the built-in function
symbols have  auto-generated types for operating on such alternate universes, as well as for operating simultaneously
on objects from different such universes.

In SAFE, there are 3 alternate universes:
1. Share counts - This universe has a copy of the natural numbers (also its positive subset, but ignore that
detail for now). It has no name for the reals.
2. Dollar amounts - this universe has a copy of the nonnegative reals (and its positive subset, but again ignore that
detail for now). It has no name for the integer subset of that set.
3. Share prices - This universe has a copy of the positive reals, "viewed as" ratios of dollar amounts per share.

Let's refer to the SMU as U0, and the others as U1, U2, U3.

"""

# arbitrary arity function type
def aafntype(dom:Sort, ran:Sort) -> ArbArityFnType:
    return ArbArityFnType(dom, ran)

# simple function type
def sfntype(*tp:Sort) -> SimpleFnType:
    assert len(tp) >= 1
    return SimpleFnType(tp)

X = 'XVar'
N = 'NVar'
D = 'DVar'
R = 'RVar'
typevars = {X, N, D, R}
j = 'jvar'

TypeData = Sequence[Any]

Natj = SApp('Dup',Nat,j)
PosIntj = SApp('Dup',PosInt,j)

# def dNat(var:str) -> Tuple[str,...]:
#     return ('Dup',Nat,var)

# nl for nested list
# def flatten_helper(nl: Sequence[Any]) -> Iterable[TypeData]:
#     # print(nl)
#     if nl[0] == 'fn' or nl[0] == 'aafn':
#         return [nl]
#     else:
#         # rv = []
#         # for inner in nl:
#         #     rv.extend(flatten_helper(inner))
#         # return rv
#         return (ft for fts in flatten_helper(nl) for ft in fts)

def parametric_one_var(tp_or_tps:Union[NonoverloadedFnType,Iterable[NonoverloadedFnType]],
                       substitutions:Iterable[Sort] = AllSortData,
                       var:str = X) -> Iterable[NonoverloadedFnType]:
    if isinstance(tp_or_tps, SimpleFnType):
        for sort in substitutions:
            yield tp_or_tps.subst(var,sort)
    elif isinstance(tp_or_tps, ArbArityFnType):
        for sort in substitutions:
            yield substarb(tp_or_tps,var,sort)
    else:
        for tp in tp_or_tps:
            for sort in substitutions:
                yield tp.subst(var,sort)

    # tps : Set[TypeData]
    # if isinstance(tp_or_tps, Set):
    #     tps = tp_or_tps
    # else:
    #     tps = {tp_or_tps}
    # rv = []
    # for tp in tps:
    #     rv.extend( [ nested_list_replace(tp, var, primtype) for primtype in substitutions ] )
    # return flatten_helper( nested_list_replace(tp, var, primtype) for )

def parametric_mult_vars(tps:Iterable[NonoverloadedFnType],
                         substitutions:Iterable[Dict[str, Sort]]) -> Iterable[NonoverloadedFnType]:
    for tp in tps:
        for d in substitutions:
            yield tp.substdict(d)


# def dupmult(tp_or_tps:Union[TypeData,Set[TypeData]], substitutions:Iterable[Dict[str, str]]) -> Sequence[Any]:
#     tps: Set[TypeData]
#     if isinstance(tp_or_tps, Set):
#         tps = tp_or_tps
#     else:
#         tps = {tp_or_tps}
#     rv = []
#     for tp in tps:
#         rv.extend([nested_list_replace_mult(tp, subst) for subst in substitutions])
#     raise NotImplementedError

FnTypesData = List[ Tuple[Tuple[AtomicSort,...], Iterable[NonoverloadedFnType]] ]

# def mychain(*iterables:Iterable[T]) -> Iterable[T]:
#     for iter in iterables:
#         for x in iter:
#             yield x

def print_types_map(fntypesmap:Dict[str,OverloadedFnType]):
    for symb in fntypes_map:
        print("\n"+symb)
        print(str(fntypesmap[symb]))
    print(sum(len(fntypesmap[f]) for f in fntypesmap), "simple function types total.")

def makeNiceFnTypeMap(data: FnTypesData) -> Dict[str,OverloadedFnType]:
    # toiter : Dict[str,Iterable[NonoverloadedFnType]] = dict()
    start = time.process_time()
    dict_of_tuples : Dict[str,Tuple[NonoverloadedFnType,...]] = dict()
    for part in data:
        tuple_from_iter = tuple(part[1])
        for symb in part[0]:
            if symb not in dict_of_tuples:
                dict_of_tuples[symb] = tuple_from_iter
            else:
                # no time performance improvement over just tuples, but might save space?
                dict_of_tuples[symb] = tuple_from_iter + dict_of_tuples[symb]

    rv = { symb: OverloadedFnType( list(dict_of_tuples[symb]), dict(), set() ) for symb in dict_of_tuples }
    print("TIME", 1000*(time.process_time() - start))
    return rv

# def makeNiceFnTypeMap(data: FnTypesData) -> Dict[str,OverloadedFnType]:
#     # toiter : Dict[str,Iterable[NonoverloadedFnType]] = dict()
#     start = time.process_time()
#     dict_of_tuples : Dict[str,Tuple[NonoverloadedFnType,...]] = dict()
#     for part in data:
#         tuple_from_iter = tuple(part[1])
#         for symb in part[0]:
#             if symb not in dict_of_tuples:
#                 dict_of_tuples[symb] = tuple_from_iter
#             else:
#                 # no time performance improvement over just tuples, but might save space?
#                 dict_of_tuples[symb] = chain(tuple_from_iter, dict_of_tuples[symb])
#
#     rv = { symb: OverloadedFnType( list(dict_of_tuples[symb]), dict(), set() ) for symb in dict_of_tuples }
#     print("TIME", 1000*(time.process_time() - start))
#     return rv

# this version is just as fast
# def makeNiceFnTypeMap(data: FnTypesData) -> Dict[str,OverloadedFnType]:
#     # toiter : Dict[str,Iterable[NonoverloadedFnType]] = dict()
#     start = time.process_time()
#     dict_of_tuples : Dict[str,Tuple[NonoverloadedFnType,...]] = dict()
#     for part in data:
#         tuple_from_iter = list(part[1])
#         for symb in part[0]:
#             if symb not in dict_of_tuples:
#                 dict_of_tuples[symb] = tuple_from_iter if len(part[0]) == 1 else tuple_from_iter.copy()
#             else:
#                 dict_of_tuples[symb].extend(tuple_from_iter)
#
#     rv = { symb: OverloadedFnType( list(dict_of_tuples[symb]), dict(), set() ) for symb in dict_of_tuples }
#     print("TIME", 1000*(time.process_time() - start))
#     return rv

overloaded_types_data : FnTypesData = [

    # ------------TimeDelta environment variables------------
    (('event_td','next_event_td','future_event_td','sectionEntrance_td','monthStartDay_td','monthEndDay_td','contractStart_td'), (
        sfntype(TimeDelta), )
     ),
    # ------------TimeDelta fns------------
    (('days',), (
        sfntype(Nat,TimeDelta),
        sfntype(PosInt,PosTimeDelta))
     ),

    # ------------Tuples------------
    (('tuple',), parametric_one_var(
            sfntype(X, X, SApp('Tuple', X, X)),
            AllAtomicSorts)
     ),
    (('tupleGet',), parametric_one_var(
            sfntype(SApp('Tuple', X, X), '{0,1}', X),
            AllAtomicSorts)
     ),

    # ------------TimeDelta Maps------------
    (('mapSet',), parametric_one_var( (
        sfntype('EmptyTDMap', X, 'TimeDelta', SApp('TDMap', X)),
        sfntype(SApp('TDMap', X), X, 'TimeDelta', SApp('TDMap', X))
        ),
        TDMapKeySortData)
     ),
    (('tdGEQ',), parametric_one_var(
        sfntype(SApp('TDMap', X), X, 'TimeDelta', 'Bool'),
        TDMapKeySortData)
     ),
    (('mapDelete',), parametric_one_var(
        sfntype(SApp('TDMap', X), X, SApp('TDMap', X)),
        TDMapKeySortData)
     ),
    (('mapHas',), parametric_one_var(
        sfntype(SApp('TDMap', X), X, 'Bool'),
        TDMapKeySortData)
     ),
    (('nonempty','empty'), parametric_one_var(
        sfntype(SApp('TDMap', X), 'Bool'),
        TDMapKeySortData))
    ,
    (('emptyTDMap',), (sfntype('EmptyTDMap'),)),
    # has intersection problem:
    # (('emptyTDMap',), parametric_one_var(
    #     sfntype(SApp('TDMap', X)),
    #     TDMapKeySortData)
    #  ),

    # ------------Comparison and (in)equality------------
    (('≤', '≥', '<', '>'), (
        aafntype(Real, Bool),
        aafntype(PosReal, Bool),
        aafntype(TimeDelta, Bool),
        aafntype(DateTime, Bool) )
     ),
    (('==',), parametric_one_var(
        aafntype(X, Bool),
        AllAtomicSorts)
     ),
    (('!=',), parametric_one_var(
        sfntype(X, X, Bool),
        AllAtomicSorts)
     ),

    # ------------Boolean------------
     (('not',), (sfntype(Bool, Bool),)),
     (('and', 'or'), (sfntype(Bool, Bool, Bool),)),
     (('and*', 'or*'), (aafntype(Bool, Bool),)),
     (('ifthenelse',), parametric_one_var(
        sfntype(Bool, X, X, X),
        AllAtomicSorts)
     ),

    # ------------Arithmetic------------
    # TODO should apply for arbitrary Ratio sorts:
    (('min','max','+','*') , parametric_one_var(
        (aafntype(X, X),sfntype(X,X)),
        UnboundedNumericSorts.union(
            {TimeDelta, SApp('Ratio', PosReal, PosInt)})
        )
     ),
    # TODO should be arbitrary arity, and order-invariant
    (('max',), (
        sfntype(PosReal, Real, PosReal),
        sfntype(PosInt, Int, PosInt))
     ),
    # TODO: {0},{1}, and {0,1}.
    (('+',), (
        sfntype(PosIntj,Natj,PosIntj),
        sfntype(Natj,PosIntj,PosIntj),

        sfntype(PosInt,Nat,PosInt),
        sfntype(Nat,PosInt,PosInt),
        sfntype(PosReal,NonnegReal,PosReal),
        sfntype(NonnegReal,PosReal,PosReal) )
     ),
    (('*',), ( # scaling a TimeDelta
        sfntype(TimeDelta, Nat, TimeDelta),
         sfntype(Nat, TimeDelta, TimeDelta) )
     ),
    # temp hack
    (('*',), parametric_mult_vars(
        {sfntype(SApp('Ratio', N, D), D, R),
         sfntype(D, SApp('Ratio', N, D), R)},
        [
                {'NVar':Real,'DVar':PosReal, 'RVar':Real},
                {'NVar':PosReal, 'DVar': PosReal, 'RVar':PosReal},
                {'NVar':NonnegReal, 'DVar': PosReal, 'RVar':NonnegReal},
                {'NVar':Real,'DVar':PosInt, 'RVar':Real},
                {'NVar':PosReal, 'DVar': PosInt, 'RVar':PosReal},
                {'NVar':NonnegReal, 'DVar': PosInt, 'RVar':NonnegReal}
        ]
        )
     ),
    (('*',), (
        sfntype(SApp('Ratio', PosReal, PosInt), PosReal, SApp('Ratio', PosReal, PosInt)),
        sfntype(PosReal, SApp('Ratio', PosReal, PosInt), SApp('Ratio', PosReal, PosInt)) ),
     ),

    (('-',), parametric_one_var(
        sfntype(X, X, X), (Int, Real, TimeDelta))
     ),

    (('/',), parametric_mult_vars(
        {sfntype(N, D, R)},
        [
            {'NVar':Real, 'DVar':PosReal, 'RVar':SApp('Ratio',Real,PosReal)},
            {'NVar':PosReal, 'DVar':PosReal, 'RVar':SApp('Ratio',PosReal,PosReal)},
            {'NVar':NonnegReal, 'DVar':PosReal, 'RVar':SApp('Ratio',NonnegReal,PosReal)},
            {'NVar':Real, 'DVar':PosInt, 'RVar':SApp('Ratio',Real,PosInt)},
            {'NVar':PosReal, 'DVar':PosInt, 'RVar':SApp('Ratio',PosReal,PosInt)},
            {'NVar':NonnegReal, 'DVar':PosInt, 'RVar':SApp('Ratio',NonnegReal,PosInt)}
        ])
     ),
     (('/',), parametric_mult_vars(
         {sfntype(N, SApp('Ratio', N, D), R)}, [
                {'NVar':Real,'DVar':PosReal, 'RVar':Real},
                {'NVar':PosReal, 'DVar': PosReal, 'RVar':PosReal},
                {'NVar':NonnegReal, 'DVar': PosReal, 'RVar':NonnegReal},
                {'NVar':Real,'DVar':PosInt, 'RVar':Real},
                {'NVar':PosReal, 'DVar': PosInt, 'RVar':PosReal},
                {'NVar':NonnegReal, 'DVar': PosInt, 'RVar':NonnegReal},
            ])
    ),

    (('/',), (sfntype(PosTimeDelta, PosTimeDelta, PosReal),
              sfntype(TimeDelta, PosTimeDelta, NonnegReal) )

     ),

    (('^',), ( sfntype(PosReal,Real,PosReal),
                sfntype(PosInt,Nat,PosInt) )
     ),

    (('floor','round','ceil'), (
        sfntype(Real, Int),
        sfntype(PosReal, Nat),
        sfntype(NonnegReal, Nat) )
     ),
    (('ceil',), (
        sfntype(Real, Int),
        sfntype(PosReal, PosInt),
        sfntype(NonnegReal, Nat) )
     ),
    (('even','odd'), parametric_one_var(
        sfntype(X, Bool),
        (Int, Nat, PosInt) )
     )
]

unbounded_arity_fnsymbols = {'≤', '≥', '<', '>', '==', 'or*', 'and*',
                              'min','max','+','*'}

fntypes_map = makeNiceFnTypeMap(overloaded_types_data)

print_types_map(fntypes_map)

# def flatten_fntype_data(_overloaded_types_data:Iterable[ Tuple[Sequence[str], Any]]) -> Dict[str, List[TypeData]]:
#
#     _fntypes_map : Dict[str, List[Sequence[Any]]] = dict()
#     for pair in _overloaded_types_data:
#         fst = pair[0]
#         snd : Any = pair[1]
#         if isinstance(fst,str):
#             _fntypes_map[fst] = flatten_helper(snd)
#         else:
#             flattened_rhs = flatten_helper(snd)
#             for symb in fst:
#                 if symb in _fntypes_map:
#                     _fntypes_map[symb] = _fntypes_map[symb] + list(flattened_rhs)
#                 else:
#                     _fntypes_map[symb] = list(flattened_rhs)
#
#     return _fntypes_map
#
#
# def eliminate_unbounded_arity(arity_occurrences: Dict[str,Set[int]], fntypes_map: Dict[str, List[Sequence[Any]]]) -> None:
#     for f in fntypes_map:
#         # if len(arity_occurrences[f]) == 0:
#         #     continue
#
#         ftypes = fntypes_map[f]
#         for i in range(len(ftypes)-1,-1,-1):
#             ftype = ftypes[i]
#             if ftype[0] != 'aafn':
#                 continue
#             assert len(ftype) == 3
#             del ftypes[i]
#             if f in arity_occurrences:
#                 dom = ftype[1]
#                 ran = ftype[2]
#                 for arity in arity_occurrences[f]:
#                     ftypes.append(('fn',) + (dom,)*arity + (ran,))

# def temp_normalize_sorts(prog:L4Contract) -> None:
#     for eliminated_sort in TEMP_SORT_IDENTIFICATION:
#         prog.sorts.remove(eliminated_sort)
#
#     def temp_normalize_sort(s: Sort) -> Sort:
#         if s in TEMP_SORT_IDENTIFICATION:
#             return TEMP_SORT_IDENTIFICATION[s]
#         else:
#             return s

#
#
# def check_type_vars_gone():
#     for fntype_data in fntype_data_map.values():
#         assert isinstance(fntype_data, list)
#         for nonover_fntype_data in fntype_data:
#             assert isinstance(nonover_fntype_data, tuple)
#             for x in nonover_fntype_data:
#                 assert isinstance(x, str) or isinstance(x, tuple)
#                 if isinstance(x,str):
#                     assert x not in typevars, x
#                 if isinstance(x, tuple):
#                     for y in x:
#                         assert  y not in typevars, str(y) + " and " + str(nonover_fntype_data)
#
#
# def sortdata_to_sort(data:Any) -> Sort:
#     if isinstance(data,str):
#         return data
#     else:
#         return NonatomicSort(data[0], tuple(sortdata_to_sort(x) for x in data[1:]))
#
# def nonoverloaded_fntype_data_to_object(data:Any) -> NonoverloadedFnType:
#     if data[0] == 'fn':
#         return SimpleFnType(tuple(sortdata_to_sort(x) for x in data[1:]))
#     else:
#         assert data[0] == 'aafn' and len(data) == 3, data
#         return ArbArityFnType(sortdata_to_sort(data[1]), sortdata_to_sort(data[2]))

# def makeNiceFnTypeMap() -> Dict[str,OverloadedFnType]:
#     fntype_map : Dict[str,OverloadedFnType] = dict()
#     for f in fntype_data_map:
#         fntype_map[f] = OverloadedFnType( [nonoverloaded_fntype_data_to_object(datapart) for datapart in fntype_data_map[f]],
#                                           dict(), set())
#     return fntype_map

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

    # for fnsymb,oft in fntypes_map.items():
#     new_nofts : List[NonoverloadedFnType] = []
#     for noft in oft.parts:
#         print(noft)
#         new_nofts.extend(copy_sft_if_allowed(noft))
#     if len(new_nofts) > 0:
#         print("some new ones")
#         oft.parts.extend(new_nofts)