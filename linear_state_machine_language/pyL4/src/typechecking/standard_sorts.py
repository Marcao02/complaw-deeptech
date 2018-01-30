from src.independent.typing_imports import *

from src.model.Sort import *

SortOps = {'TDMap','Ratio','Tuple','Copy'}

DateTime = 'DateTime'
TimeDelta = 'TimeDelta'
PosTimeDelta = 'PosTimeDelta'
Int = 'Int'
PosInt = 'PosInt'
Nat = 'Nat'
Real = 'Real'
PosReal = 'PosReal'
NonnegReal = 'NonnegReal'
Bool = 'Bool'

def SApp(symb:str, *args:Any) -> NonatomicSort:
    return NonatomicSort.c(symb, tuple(args))

jvar = 'jvar'
Natj = SApp('Dup', Nat, jvar)
PosIntj = SApp('Dup', PosInt, jvar)
NonnegRealj = SApp('Dup', NonnegReal, jvar)
PosRealj = SApp('Dup', PosReal, jvar)


# all_sort_copies_by_orig : Dict[Sort, Set[Sort]] = dict()
# all_sort_copies : Set[Sort] = set()
def Dup(sort:Sort, name:str) -> NonatomicSort:
    rv = SApp('Dup',sort,name)
    # dictSetOrAdd(all_sort_copies_by_orig, sort, rv)
    # all_sort_copies.add(rv)
    return rv
#
# dups_used : Dict[str,Sort] = {
#     '$': Dup(NonnegReal,'$'),
#     'Pos$': Dup(PosReal,'Pos$'),
#     'ShareCnt': Dup(Nat,'ShareCnt'),
#     'PosShareCnt': Dup(PosInt,'PosShareCnt')
# }
# dups_used.update({
#     'SharePrice': Dup(SApp('Ratio', dups_used['$'], dups_used['PosShareCnt']),'SharePrice'),
#     'Order' : Dup(SApp('Tuple', Nat, Nat),'Order') })
# dups_used.update({
#     'TDMap_Order': Dup(SApp('TDMap', dups_used['Order']),'TDMap_Order')
# })

# todo_once("this is temporary too")
# TEMP_SORT_IDENTIFICATION: Dict[Sort, Sort] = {
#
#     # '$': NonnegReal,
#     # '$': Dup(NonnegReal,"$"),
#
#     # 'Pos$': PosReal,
#     # 'Pos$': Dup(PosReal,'Pos$'),
#
#     # 'ShareCnt': Nat,
#     # 'Shares' : Dup(Nat,'Shares'),
#
#     # 'PosShareCnt': PosInt,
#     # 'PosShares' : Dup(PosInt,'PosShares'),
#
#     # 'SharePrice': SApp('Ratio', PosReal, PosInt),
#     'SharePrice': SApp('Ratio', Dup(NonnegReal,'$'), Dup(PosInt,'PosShareCnt')),
#
#     # 'SharePrice': SApp('Ratio', PosReal, PosInt),
#     'PosSharePrice': SApp('Ratio', Dup(PosReal,'Pos$'), Dup(PosInt,'PosShareCnt')),
#
#     'Order': SApp('Tuple', Nat, Nat),
#     # 'Order' : Dup(SApp('Tuple', Nat, Nat),'Order'),
#     'TDMap_Order': SApp('TDMap', SApp('Tuple', Nat, Nat)),
#     # 'TDMap_Order': SApp('TDMap', Dup(SApp('Tuple',Nat,Nat),'Order')),
# }


def Ratio(s1,s2):
    return SApp('Ratio',s1,s2)

RatioAtomicSorts : Set[Sort] = { Ratio(PosRealj,PosIntj), Ratio(NonnegRealj,PosIntj) }

NormalUnboundedNumericSorts = cast(Set[Sort],{Int,Nat,PosInt,Real,NonnegReal,PosReal})

UnboundedNumericSorts = NormalUnboundedNumericSorts\
                            .union( cast(Set[Sort],{PosIntj,PosRealj,Natj,NonnegRealj} ) )\
                            .union( RatioAtomicSorts)
# UnboundedNumericSortCopies : Set[Sort] = set()
# for orig in UnboundedNumericSorts:
#     if orig in all_sort_copies_by_orig:
#         UnboundedNumericSortCopies.union(all_sort_copies_by_orig[orig])

BoundedNumericSorts = {"[0,1]","(0,1]","[0,1)","(0,1)"}

# Because all non-empty intersections must be represented (for now)
FiniteNumericSorts = {"{0,1}","{0}","{1}"}

AllNumericSorts = UnboundedNumericSorts.union(BoundedNumericSorts).union(FiniteNumericSorts)

AllAtomicSorts : Set[Sort] = cast(Set[Sort],{DateTime,TimeDelta,PosTimeDelta,Bool,'EmptyTDMap'})\
                                .union(AllNumericSorts)

TupleAtomicSorts : Set[Sort] = {SApp('Tuple',S,S) for S in AllAtomicSorts}
# TupleAtomicSorts = map(lambda t: SApp("Tuple",t) ,TupleAtomicSortData)

TDMapKeySorts = TupleAtomicSorts.union(AllAtomicSorts)
TDMapSorts = map(lambda t: SApp("TDMap", t), TDMapKeySorts)

AllSorts : Set[Any] = AllAtomicSorts\
                        .union( TupleAtomicSorts )\
                        .union( TDMapSorts ) \
                        .union( RatioAtomicSorts )
                        # .union( set(TEMP_SORT_IDENTIFICATION.values()) )\

