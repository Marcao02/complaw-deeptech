from typing import NamedTuple, Tuple, Any, Union, cast, NewType, Optional, Dict, Set

from src.model.Sort import *
from src.util import todo_once, mapjoin, dictSetOrAdd

SortOps = {'TDMap','Rate','Tuple','Copy'}

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
    return NonatomicSort.mk(symb, tuple(args))

todo_once("this is temporary too")
TEMP_SORT_IDENTIFICATION: Dict[Sort, Sort] = {
    'ℚ': Real,
    'ℕ': Nat,
    '$': NonnegReal,
    'Pos$': PosReal,
    # 'Pos$': Copy(PosReal,'Pos$'),

    'Shares': Nat,
    # 'Shares' : Copy(Nat,'Shares'),
    'PosShares': PosInt,
    # 'PosShares' : Copy(PosInt,'PosShares'),

    '$/Shares': SApp('Rate', PosReal, PosInt),
    # '$/Shares': SApp('Rate', Copy(PosReal,'Pos$'), Copy(PosInt,'PosShares')),
    'Order': SApp('Tuple', Nat, Nat),
    # 'Order' : Copy(SApp('Tuple', Nat, Nat),'Order'),
    'TDMap_Order': SApp('TDMap', SApp('Tuple', Nat, Nat)),
    # 'TDMap_Order': SApp('TDMap', Copy(SApp('Tuple',Nat,Nat),'Order')),

    '%': "[0,1]"
}

all_sort_copies_by_orig : Dict[Sort, Set[Sort]] = dict()
all_sort_copies : Set[Sort] = set()
def Copy(sort:Sort, name:str) -> NonatomicSort:
    rv = SApp('Copy',sort,name)
    dictSetOrAdd(all_sort_copies_by_orig, sort, rv)
    all_sort_copies.add(rv)
    return rv

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

AllAtomicSorts : Set[Sort] = cast(Set[Sort],{DateTime,TimeDelta,PosTimeDelta,Bool})\
                                .union(AllNumericSorts)

TupleAtomicSortData : Set[Any] = {('Tuple',S,S) for S in AllAtomicSorts}
TDMapKeySortData = TupleAtomicSortData.union(AllAtomicSorts)
AllSortData : Set[Any] = AllAtomicSorts.union(TupleAtomicSortData).union(TDMapKeySortData)

TupleAtomicSorts = map(SApp,TupleAtomicSortData)
TDMapKeySorts = map(SApp,TDMapKeySortData)

AllSorts : Set[Any] = AllAtomicSorts\
                        .union( set(TEMP_SORT_IDENTIFICATION.values()) )\
                        .union( TupleAtomicSorts )\
                        .union( TDMapKeySorts )

