import time

from src.independent.typing_imports import *
from src.model.Sort import Sort, SortOpApp, AtomicSort
from src.model.FnTypes import OverloadedFnType, SimpleFnType, SimpleFnType
from src.typechecking.standard_sorts import AtomicSortsAndDimensionedNumericSorts, TDMapKeySorts, TimeDelta, Bool, \
    UnboundedNumericSorts, \
    PosInt, PosReal, Int, NonnegReal, Nat, PosTimeDelta, BoundedRealIntervalSorts, DateTime, Real, \
    AllNumericSorts, SApp, NonnegReal2, Nat1, PosInt1, PosReal2, AllSorts, Ratio, Real2, Int1

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


ASSOCIATIVE_OPS = {'or', 'and', 'min','max','+','*'}
CHAIN_PREDS = {'≤', '≥', '<', '>', '=='}

# arbitrary arity function type... which are now handled by virtual reduction to their binary operations in typecheck.py
# def aafntype(dom:Sort, ran:Sort) -> ArbArityFnType:
#     return ArbArityFnType(dom, ran)
def aafntype(dom:Sort, ran:Sort) -> SimpleFnType:
    return SimpleFnType((dom, dom, ran))

# simple function type
def sfntype(*tp:Sort) -> SimpleFnType:
    assert len(tp) >= 1
    return SimpleFnType(tp)

FnTypesData = List[ Tuple[Tuple[AtomicSort,...], Iterable[SimpleFnType]] ]
TypeData = Sequence[Any]
FnTypesMap = Dict[str, OverloadedFnType]

X = 'XVar'
N = 'NVar'
D = 'DVar'
R = 'RVar'
TYPEVARS = {X, N, D, R}


def parametric_one_var(tp_or_tps:Union[SimpleFnType,Iterable[SimpleFnType]],
                       substitutions:Iterable[Sort] = AllSorts,
                       var:str = X) -> Iterable[SimpleFnType]:
    if isinstance(tp_or_tps, SimpleFnType):
        for sort in substitutions:
            yield tp_or_tps.subst(var,sort)
    # elif isinstance(tp_or_tps, ArbArityFnType):
    #     for sort in substitutions:
    #         yield tp_or_tps.substForVar(var,sort)
    else:
        for tp in tp_or_tps:
            for sort in substitutions:
                yield tp.subst(var,sort)

def parametric_mult_vars(tps:Iterable[SimpleFnType],
                         substitutions:Iterable[Dict[str, Sort]]) -> Iterable[SimpleFnType]:
    for tp in tps:
        for d in substitutions:
            yield tp.substdict(cast(Dict[Sort,Sort],d)) # false cast, but sound as long as we don't modify the dict.

def print_types_map(fntypesmap:FnTypesMap):
    for symb in STANDARD_FNTYPES:
        print("\n"+symb)
        print(str(fntypesmap[symb]))
    print(sum(len(fntypesmap[f]) for f in fntypesmap), "simple function types total.")

def makeNiceFnTypeMap(data: FnTypesData) -> FnTypesMap:
    # toiter : Dict[str,Iterable[SimpleFnType]] = dict()
    # start = time.process_time()
    dict_of_tuples : Dict[str,Tuple[SimpleFnType,...]] = dict()
    for part in data:
        tuple_from_iter = tuple(part[1])
        for symb in part[0]:
            if symb not in dict_of_tuples:
                dict_of_tuples[symb] = tuple_from_iter
            else:
                # no time performance improvement over just tuples, but might save space?
                dict_of_tuples[symb] = tuple_from_iter + dict_of_tuples[symb]

    rv = { symb: OverloadedFnType( set(dict_of_tuples[symb]), dict() ) for symb in dict_of_tuples }
    # print("TIME", 10*(time.process_time() - start))
    return rv

    # speed the same
    # def makeNiceFnTypeMap(data: FnTypesData) -> FnTypesMap:
    #     # toiter : Dict[str,Iterable[SimpleFnType]] = dict()
    #     start = time.process_time()
    #     dict_of_tuples : Dict[str,Tuple[SimpleFnType,...]] = dict()
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
    # def makeNiceFnTypeMap(data: FnTypesData) -> FnTypesMap:
    #     # toiter : Dict[str,Iterable[SimpleFnType]] = dict()
    #     start = time.process_time()
    #     dict_of_tuples : Dict[str,Tuple[SimpleFnType,...]] = dict()
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
def check_type_vars_gone(oftmap:FnTypesMap):
    for oft in oftmap.values():
        for noft in oft.parts:
            for x in noft.parts:
                check_type_vars_gone_from_sort(x)


overloaded_types_data : FnTypesData = [
    # ------------TimeDelta environment variables------------
    (('last_event_td','next_event_td','future_event_td','last_situation_td','monthStartDay_td','monthEndDay_td','contractStart_td'), (
        sfntype(TimeDelta), )
     ),
    # ------------TimeDelta fns------------
    (('days',), (
        sfntype(Nat,TimeDelta),
        sfntype(PosInt,PosTimeDelta))
     ),
    # ------------DateTime environment variables------------
    (('event_dt','next_event_dt'), (
        # sfntype(DateTime), )
        sfntype(TimeDelta), )
     ),
    # ------------Event role getter------------
    (('event_role',), (
        sfntype('RoleId'),)
     ),

    # ------------Tuples------------
    # generalized and moved into typecheck.py
    # (('tuple',), parametric_one_var(
    #         sfntype(X, X, SApp('Tuple', X, X)),
    #         AtomicSortsAndDimensionedNumericSorts)
    #  ),
    # (('tupleGet',), parametric_one_var(
    #         sfntype(SApp('Tuple', X, X), '{0,1}', X),
    #         AtomicSortsAndDimensionedNumericSorts)
    #  ),

    # ------------TimeDelta Maps------------
    (('mapSet',), parametric_one_var( (
        # sfntype('EmptyTDMap',     X, 'TimeDelta', SApp('TDMap', X)), # redundant given EmptyTDMap ≤ TDMap[X]
        sfntype(SApp('TDMap', X), X, 'TimeDelta', SApp('TDMap', X))
        ),
        TDMapKeySorts)
     ),
    (('tdGEQ',), parametric_one_var(
        sfntype(SApp('TDMap', X), X, 'TimeDelta', 'Bool'),
        TDMapKeySorts)
     ),
    (('delete',), parametric_one_var(
        sfntype(SApp('TDMap', X), X, SApp('TDMap', X)),
        TDMapKeySorts)
     ),
    (('has',), parametric_one_var(
        sfntype(SApp('TDMap', X), X, 'Bool'),
        TDMapKeySorts)
     ),
    (('tdmapHasItemExpiredBefore',), parametric_one_var(
        sfntype(SApp('TDMap', X), 'TimeDelta', 'Bool'),
        TDMapKeySorts)
     ),
    (('nonempty','empty'), parametric_one_var(
        sfntype(SApp('TDMap', X), 'Bool'),
        TDMapKeySorts))
    ,
    (('minValue',), parametric_one_var(
        sfntype(SApp('TDMap', X), 'TimeDelta'),
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

        aafntype(NonnegReal2, Bool),
        aafntype(Nat1, Bool),
        aafntype(Ratio(NonnegReal2, PosInt1), Bool),

        )
     ),
    (('==',), parametric_one_var(
        aafntype(X, Bool),
        AtomicSortsAndDimensionedNumericSorts)
     ),
    (('!=',), parametric_one_var(
        sfntype(X, X, Bool),
        AtomicSortsAndDimensionedNumericSorts)
     ),

    # ------------Boolean------------
     (('not',), (sfntype(Bool, Bool),)),
     (('and', 'or'), (sfntype(Bool, Bool, Bool),)),
     (('and', 'or'), (aafntype(Bool, Bool),)),
     (('ifthenelse',), parametric_one_var(
        sfntype(Bool, X, X, X),
        AtomicSortsAndDimensionedNumericSorts)
     ),

    # ------------Arithmetic------------
    (('min','max','+','*') , parametric_one_var(
        (aafntype(X,X),),
        UnboundedNumericSorts\
            .union({TimeDelta})
            .union({NonnegReal2, Real2, Nat1, Int1})
        )
     ),
    (('max',), (
        sfntype(PosReal, Real, PosReal),
        sfntype(Real,PosReal, PosReal),
        sfntype(PosReal2, Real2, PosReal2),
        sfntype(Real2,PosReal2, PosReal2),

        sfntype(PosInt, Int, PosInt),
        sfntype(Int, PosInt, PosInt),

        sfntype(NonnegReal2, Real2, NonnegReal2),
        sfntype(Real2,NonnegReal2, NonnegReal2),

        sfntype(Nat, Int, Nat),
        sfntype(Int, Nat, Nat)
        )
    ),
    # to do: {0},{1}, and {0,1}.
    (('+',), (
        sfntype(PosInt, Nat, PosInt),
        sfntype(Nat, PosInt, PosInt),
        sfntype(PosReal, NonnegReal, PosReal),
        sfntype(NonnegReal, PosReal, PosReal),

        sfntype(PosInt1, Nat1, PosInt1),
        sfntype(Nat1, PosInt1, PosInt1),
        sfntype(PosReal2, NonnegReal2, PosReal2),
        sfntype(NonnegReal2, PosReal2, PosReal2),

        sfntype(DateTime, TimeDelta, DateTime)
    )
    ),
    (('*',), (  # scaling things
        sfntype(TimeDelta, Nat, TimeDelta),
        sfntype(Nat, TimeDelta, TimeDelta),
        sfntype(NonnegReal2, NonnegReal, NonnegReal2),
        sfntype(NonnegReal, NonnegReal2, NonnegReal2),
        sfntype(Ratio(NonnegReal2, PosInt1), PosReal, Ratio(NonnegReal2, PosInt1)),
        sfntype(Ratio(PosReal2, PosInt1), PosReal, Ratio(PosReal2, PosInt1)),
        )
     ),

    (('*',), parametric_mult_vars(
        {sfntype(X, R, X), sfntype(R, X, X)},
        [{X: somenumeric, R: PosReal} for somenumeric in cast(Set[Sort], {NonnegReal2, PosReal2, Real2})]
    )
     ),

    (('*',), parametric_mult_vars(
        {sfntype(X, R, X), sfntype(R, X, X)},
        [{X: somenumeric, R: PosInt} for somenumeric in cast(Set[Sort], {Nat1, PosInt1, Int1})]
    )
     ),

    (('*',), (
        sfntype(Ratio( PosReal, PosInt), PosReal, Ratio( PosReal, PosInt)),
        sfntype(PosReal, Ratio( PosReal, PosInt), Ratio( PosReal, PosInt)) ),
     ),

    (('-',), parametric_one_var(
        sfntype(X, X, X),
        (Int, Real, TimeDelta, Real2, Int1))
     ),

    (('/',), parametric_mult_vars(
        {sfntype(N, D, R)},
        [
            {N:Real, D:PosReal, R:Ratio(Real,PosReal)},
            {N:PosReal, D:PosReal, R:Ratio(PosReal,PosReal)},
            {N:NonnegReal, D:PosReal, R:Ratio(NonnegReal,PosReal)},
            {N:Real, D:PosInt, R:Ratio(Real,PosInt)},
            {N:PosReal, D:PosInt, R:Ratio(PosReal,PosInt)},
            {N:NonnegReal, D:PosInt, R:Ratio(NonnegReal,PosInt)},

            {N:PosReal2, D:PosInt1, R:Ratio( PosReal2, PosInt1)},
            {N:NonnegReal2, D:PosInt1, R:Ratio( NonnegReal2, PosInt1)},

            {N:Nat1, D:PosInt1, R:Ratio( Nat, PosInt)},
            {N:PosInt1, D:PosInt1, R:Ratio( PosInt, PosInt)},
            # apparently haven't actually used these:
            {N:PosReal2, D:PosReal2, R:PosReal},
            {N:NonnegReal2, D:PosReal2, R:NonnegReal},

            {N:PosReal2, D:PosInt, R:PosReal2}
        ])
     ),
     (('/',), parametric_mult_vars(
         {sfntype(N, Ratio( N, D), R)}, [
                {N:Real,D:PosReal, R:Real},
                {N:PosReal, D: PosReal, R:PosReal},
                {N:NonnegReal, D: PosReal, R:NonnegReal},
                {N:Real,D:PosInt, R:Real},
                {N:PosReal, D: PosInt, R:PosReal},
                {N:NonnegReal, D: PosInt, R:NonnegReal},
            ])
    ),

    (('/',), (sfntype(PosTimeDelta, PosTimeDelta, PosReal),
              sfntype(TimeDelta, PosTimeDelta, NonnegReal) )
     ),

    (('floor/','round/'),  parametric_mult_vars(
         {sfntype(N, R, D)}, [
                {N:Nat, R:PosInt, D:Nat},
                {N:NonnegReal2, R: Ratio(NonnegReal2,PosInt1), D:Nat1},
                # {N:PosReal2, R: Ratio(PosReal2,PosInt1), D:Nat1}, # redundant
                # {N:NonnegReal2, D: PosInt1, R:NonnegReal2},
                # {N:NonnegReal2, D: PosReal2, R: NonnegReal2}
            ])
    ),

    (('ceil/',),  parametric_mult_vars(
         {sfntype(N, R, D)}, [
                {N:PosReal, R:PosReal, D:PosInt},
                {N:Nat, R:PosInt, D:Nat},
                {N:NonnegReal2, R: Ratio(NonnegReal2,PosInt1), D:Nat1},
                {N:PosReal2, R: Ratio(PosReal2,PosInt1), D:PosInt1}
            ])
    ),


    (('fraction-of-sum',), (
        sfntype(PosInt,Nat,"Fraction(0,1]"),
        sfntype(PosInt,PosInt,"Fraction(0,1)"),
        sfntype(PosReal2,PosReal2,"Fraction[0,1)"),
        sfntype(Nat,PosInt,"Fraction[0,1)"),
        sfntype(PosInt1,Nat1,"Fraction(0,1]"),
        sfntype(PosInt1,PosInt1,"Fraction(0,1)"),
        sfntype(Nat1,PosInt1,"Fraction[0,1)")
    )),

    (('^',), ( sfntype(PosReal,Real,PosReal),
               sfntype(PosInt,Nat,PosInt),
               sfntype(Nat, PosInt, Nat) )
     ),

    (('floor','round','ceil'), (
        sfntype(Real, Int),
        sfntype(PosReal, Nat),
        sfntype(NonnegReal, Nat)
        )
     ),
    (('ceil',), (
        sfntype(PosReal, PosInt),
     )),
    (('even','odd'), parametric_one_var(
        sfntype(X, Bool),
        (Int, Nat, PosInt) )
     )
]


STANDARD_FNTYPES = makeNiceFnTypeMap(overloaded_types_data)
# print_types_map(fntypes_map)

check_type_vars_gone(STANDARD_FNTYPES)


# def dupmult(tp_or_tps:Union[TypeData,Set[TypeData]], substitutions:Iterable[Dict[str, str]]) -> Sequence[Any]:
#     tps: Set[TypeData]
#     if isinstance(tp_or_tps, Set):
#         tps = tp_or_tps
#     else:
#         tps = {tp_or_tps}
#     rv = []
#     for tp in tps:
#         rv.extend([nested_list_replace_mult(tp, substForVar) for substForVar in substitutions])
#     raise NotImplementedError

# """
# depends on dups_used. modifies first arg.
# """
# def subst_concrete_dups(fntypes: FnTypesMap):
#     newparts : List[SimpleFnType] = []
#     substForVar : Dict[Tuple[str,str], SortOpApp] = dict()
#     for fnsymb,oft in fntypes.items():
#         newparts.clear()
#         for noft in oft.parts:
#             substForVar.clear()
#             for sort in noft.parts:
#                 if isinstance(sort,SortOpApp):
#                     op = sort.op
#                     if op == 'Dimensioned':
#                         dupsort = cast(Sort,sort.args[0])
#                         dupvar = cast(str,sort.args[1])
# def isSimpleNumericFnType(ft:SimpleFnType) -> bool:
#     if isinstance(ft,ArbArityFnType):
#         return ft.dom in AllNumericSorts and ft.dom == ft.ran
#     if isinstance(ft,SimpleFnType):
#         solesort = ft.ran
#         return solesort in AllNumericSorts and all(ft.dom[i] == solesort for i in range(len(ft.dom)))
# def copy_sft_if_allowed(ft:SimpleFnType) -> Iterable[SimpleFnType]:
#     if not isSimpleNumericFnType(ft):
#         return []
#     solesort = ft.ran
#     if solesort not in all_sort_copies_by_orig:
#         return []
#
#     ft_copies : List[SimpleFnType] = []
#     # for candidate_supersort in all_sort_copies_by_orig:
#     #     if standard_types_graph.hasEdge(solesort, candidate_supersort):
#     for sort1 in all_sort_copies_by_orig[solesort]:
#         if isinstance(ft, ArbArityFnType):
#             ft_copies.append( ArbArityFnType(sort1,sort1) )
#         if isinstance(ft, SimpleFnType):
#             ft_copies.append( SimpleFnType(tuple(sort1 for inst in ft_copies)) )
#     return ft_copies
# for fnsymb,oft in fntypes_map.items():
#     new_nofts : List[SimpleFnType] = []
#     for noft in oft.parts:
#         print(noft)
#         new_nofts.extend(copy_sft_if_allowed(noft))
#     if len(new_nofts) > 0:
#         print("some new ones")
#         oft.parts.extend(new_nofts)