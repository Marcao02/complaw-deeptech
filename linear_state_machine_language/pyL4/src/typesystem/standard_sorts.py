from src.independent.typing_imports import *

from src.model.Sort import *
from src.util import todo_once, mapjoin, dictSetOrAdd

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

j = 'jvar'
Natj = SApp('Dup',Nat,j)
PosIntj = SApp('Dup',PosInt,j)
NonnegRealj = SApp('Dup',NonnegReal,j)
PosRealj = SApp('Dup',PosReal,j)


all_sort_copies_by_orig : Dict[Sort, Set[Sort]] = dict()
all_sort_copies : Set[Sort] = set()
def Dup(sort:Sort, name:str) -> NonatomicSort:
    rv = SApp('Dup',sort,name)
    # dictSetOrAdd(all_sort_copies_by_orig, sort, rv)
    # all_sort_copies.add(rv)
    return rv

dups_used : Dict[str,Sort] = {
    '$': Dup(NonnegReal,'$'),
    'Pos$': Dup(PosReal,'Pos$'),
    'Shares': Dup(Nat,'Shares'),
    'PosShares': Dup(PosInt,'PosShares')
}
dups_used.update({
    '$/Shares': Dup(SApp('Ratio', dups_used['$'], dups_used['PosShares']),'$/Shares'),
    'Order' : Dup(SApp('Tuple', Nat, Nat),'Order') })
dups_used.update({
    'TDMap_Order': Dup(SApp('TDMap', dups_used['Order']),'TDMap_Order')
})
todo_once("this is temporary too")
TEMP_SORT_IDENTIFICATION: Dict[Sort, Sort] = {

    '$': NonnegReal,
    # '$': Dup(NonnegReal,"$"),

    'Pos$': PosReal,
    # 'Pos$': Dup(PosReal,'Pos$'),

    'Shares': Nat,
    # 'Shares' : Dup(Nat,'Shares'),
    'PosShares': PosInt,
    # 'PosShares' : Dup(PosInt,'PosShares'),
    'ShareCnt': Nat,
    # 'Shares' : Dup(Nat,'Shares'),
    'PosShareCnt': PosInt,
    # 'PosShares' : Dup(PosInt,'PosShares'),

    '$/Shares': SApp('Ratio', PosReal, PosInt),
    # '$/Shares': SApp('Ratio', Dup(PosReal,'Pos$'), Dup(PosInt,'PosShares')),
    'SharePrice': SApp('Ratio', PosReal, PosInt),

    'Order': SApp('Tuple', Nat, Nat),
    # 'Order' : Dup(SApp('Tuple', Nat, Nat),'Order'),
    'TDMap_Order': SApp('TDMap', SApp('Tuple', Nat, Nat)),
    # 'TDMap_Order': SApp('TDMap', Dup(SApp('Tuple',Nat,Nat),'Order')),
}

UnboundedNumericSorts = cast(Set[Sort],{Int,Nat,PosInt,Real,NonnegReal,PosReal})
                            # .union(all_sort_copies)
UnboundedNumericSortCopies : Set[Sort] = set()
for orig in UnboundedNumericSorts:
    if orig in all_sort_copies_by_orig:
        UnboundedNumericSortCopies.union(all_sort_copies_by_orig[orig])

BoundedNumericSorts = {"[0,1]","(0,1]","[0,1)","(0,1)"}

# Because all non-empty intersections must be represented (for now)
FiniteNumericSorts = {"{0,1}","{0}","{1}"}

AllNumericSorts = UnboundedNumericSorts.union(BoundedNumericSorts).union(FiniteNumericSorts)

AllAtomicSorts : Set[Sort] = cast(Set[Sort],{DateTime,TimeDelta,PosTimeDelta,Bool,'EmptyTDMap'})\
                                .union(AllNumericSorts)

TupleAtomicSortData : Set[Sort] = {SApp('Tuple',S,S) for S in AllAtomicSorts}
TDMapKeySortData = TupleAtomicSortData.union(AllAtomicSorts)
AllSortData : Set[Sort] = AllAtomicSorts.union(TupleAtomicSortData).union(TDMapKeySortData)

TupleAtomicSorts = map(lambda t: SApp("Tuple",t) ,TupleAtomicSortData)
TDMapKeySorts = map(lambda t: SApp("TDMap",t),TDMapKeySortData)

AllSorts : Set[Any] = AllAtomicSorts\
                        .union( set(TEMP_SORT_IDENTIFICATION.values()) )\
                        .union( TupleAtomicSorts )\
                        .union( TDMapKeySorts )

