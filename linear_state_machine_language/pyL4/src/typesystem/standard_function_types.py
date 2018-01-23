import time

from src.independent.typing_imports import *
from src.model.Sort import Sort, NonatomicSort, AtomicSort
from src.model.FnTypes import OverloadedFnType, NonoverloadedFnType, SimpleFnType, ArbArityFnType
from src.typesystem.standard_sorts import AllAtomicSorts, TDMapKeySorts, TimeDelta, Bool, UnboundedNumericSorts, \
    PosInt, PosReal, Int, NonnegReal, Nat, PosTimeDelta, BoundedNumericSorts, DateTime, Real, \
    AllNumericSorts, SApp, NonnegRealj, Natj, PosIntj, PosRealj, AllSorts, Ratio
from src.temp_src.for_safe import doit_for_safe

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
3. Share prices - This universe has a copy of the positive reals, "viewed as" ratios of (2).dollar_amounts per (1).share.

Let's refer to the SMU as U0, and the others as U1, U2, U3.

"""

# arbitrary arity function type
def aafntype(dom:Sort, ran:Sort) -> ArbArityFnType:
    return ArbArityFnType(dom, ran)

# simple function type
def sfntype(*tp:Sort) -> SimpleFnType:
    assert len(tp) >= 1
    return SimpleFnType(tp)

FnTypesData = List[ Tuple[Tuple[AtomicSort,...], Iterable[NonoverloadedFnType]] ]
TypeData = Sequence[Any]

X = 'XVar'
N = 'NVar'
D = 'DVar'
R = 'RVar'
TYPEVARS = {X, N, D, R}


def parametric_one_var(tp_or_tps:Union[NonoverloadedFnType,Iterable[NonoverloadedFnType]],
                       substitutions:Iterable[Sort] = AllSorts,
                       var:str = X) -> Iterable[NonoverloadedFnType]:
    if isinstance(tp_or_tps, SimpleFnType):
        for sort in substitutions:
            yield tp_or_tps.subst(var,sort)
    elif isinstance(tp_or_tps, ArbArityFnType):
        for sort in substitutions:
            yield tp_or_tps.subst(var,sort)
    else:
        for tp in tp_or_tps:
            for sort in substitutions:
                yield tp.subst(var,sort)

def parametric_mult_vars(tps:Iterable[NonoverloadedFnType],
                         substitutions:Iterable[Dict[str, Sort]]) -> Iterable[NonoverloadedFnType]:
    for tp in tps:
        for d in substitutions:
            yield tp.substdict(cast(Dict[Sort,Sort],d)) # false cast, but sound as long as we don't modify the dict.

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

    # speed the same
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

def check_type_vars_gone_from_sort(s:Sort):
    if isinstance(s,str):
        assert s not in TYPEVARS
    else:
        for t in s.args:
            check_type_vars_gone_from_sort(t)
def check_type_vars_gone(oftmap:Dict[str,OverloadedFnType]):
    for oft in oftmap.values():
        for noft in oft.parts:
            for x in noft.parts:
                check_type_vars_gone_from_sort(x)


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
        TDMapKeySorts)
     ),
    (('tdGEQ',), parametric_one_var(
        sfntype(SApp('TDMap', X), X, 'TimeDelta', 'Bool'),
        TDMapKeySorts)
     ),
    (('mapDelete',), parametric_one_var(
        sfntype(SApp('TDMap', X), X, SApp('TDMap', X)),
        TDMapKeySorts)
     ),
    (('mapHas',), parametric_one_var(
        sfntype(SApp('TDMap', X), X, 'Bool'),
        TDMapKeySorts)
     ),
    (('nonempty','empty'), parametric_one_var(
        sfntype(SApp('TDMap', X), 'Bool'),
        TDMapKeySorts))
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
        aafntype(TimeDelta, Bool),
        aafntype(DateTime, Bool),

        aafntype(NonnegRealj, Bool),
        aafntype(Natj, Bool),

        )
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
        (aafntype(X,X),),
        UnboundedNumericSorts.union(
            {TimeDelta, SApp('Ratio', PosReal, PosInt)}).union(
            {NonnegRealj, Natj})
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
        sfntype(PosRealj,NonnegRealj,PosRealj),
        sfntype(NonnegRealj,PosRealj,PosRealj),
        sfntype(PosInt, Nat, PosInt),
        sfntype(Nat, PosInt, PosInt),
        sfntype(PosReal, NonnegReal, PosReal),
        sfntype(NonnegReal, PosReal, PosReal) )
    ),
    (('*',), (  # scaling things
        sfntype(TimeDelta, Nat, TimeDelta),
        sfntype(Nat, TimeDelta, TimeDelta),
        sfntype(NonnegRealj, NonnegReal, NonnegRealj),
        sfntype(NonnegReal, NonnegRealj, NonnegRealj),
        sfntype(Ratio(NonnegRealj,PosIntj), PosReal, Ratio(NonnegRealj,PosIntj)),
        sfntype(Ratio(PosRealj, PosIntj), PosReal, Ratio(PosRealj, PosIntj)),
        )
     ),

    (('*',), parametric_mult_vars(
        {sfntype(X, R, X), sfntype(R, X, X)},
        [{X: somenumeric, R: PosReal} for somenumeric in AllNumericSorts]
    )
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
            {'NVar':PosRealj, 'DVar':PosIntj, 'RVar':SApp('Ratio',PosRealj,PosIntj)},
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
                {'NVar':PosRealj, 'DVar': PosIntj, 'RVar':PosRealj},
                {'NVar':NonnegRealj, 'DVar': PosIntj, 'RVar':NonnegRealj},
                {'NVar':NonnegRealj, 'DVar': PosRealj, 'RVar': NonnegRealj}

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
        sfntype(NonnegReal, Nat),
        sfntype(PosRealj, Natj),  # is that ok?
        sfntype(NonnegRealj, Natj),  # is that ok?

        )
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




# obviously this can be computed from overloaded_types_data, but why bother?
unbounded_arity_fnsymbols = {'≤', '≥', '<', '>', '==', 'or*', 'and*',
                              'min','max','+','*'}

fntypes_map = makeNiceFnTypeMap(overloaded_types_data)
# print_types_map(fntypes_map)

check_type_vars_gone(fntypes_map)

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
#
#



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