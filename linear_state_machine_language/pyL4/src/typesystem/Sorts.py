from typing import NamedTuple, Tuple, Any, Union, cast, NewType, Optional, Dict, Set

from src.util import todo_once, mapjoin, dictSetOrAdd

SortOp = str # NewType('SortOp',str)
AtomicSort = str # NewType('AtomicSort',str)
Sort = Union[AtomicSort, 'NonatomicSort']
class NonatomicSort(NamedTuple):
    sortop: SortOp
    args_: Tuple[Any,...]
    @staticmethod
    def c(sortop:AtomicSort, args:Tuple[Any,...]) -> 'NonatomicSort':
        return NonatomicSort(sortop, args)
    @property
    def args(self) -> Tuple[Sort,...]:
        return cast(Tuple[Sort], self.args_)
    @staticmethod
    def mk(sortop:AtomicSort, args:Tuple[Sort,...]) -> 'NonatomicSort':
        return NonatomicSort(sortop, args)

    def __str__(self) -> str:
        if self.sortop == 'Tuple':
            return f"{mapjoin(str,self.args,'×')}"
        elif self.sortop == 'Rate':
            return f"{mapjoin(str,self.args,'/')}"
        elif self.sortop == 'Copy':
            return str(self.args[1])
        else:
            return f"{self.sortop}[{mapjoin(str,self.args,', ')}]"
    def __repr__(self) -> str:
        return str(self)

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

all_sort_copies_by_orig : Dict[Sort, Set[Sort]] = dict()
all_sort_copies : Set[Sort] = set()
def Copy(sort:Sort, name:str) -> NonatomicSort:
    rv = SApp('Copy',sort,name)
    dictSetOrAdd(all_sort_copies_by_orig, sort, rv)
    all_sort_copies.add(rv)
    return rv


todo_once("this is temporary too")
TEMP_SORT_IDENTIFICATION : Dict[Sort,Sort] = {
    'ℚ' : Real,
    'ℕ' : Nat,
    '$' : NonnegReal,
    'Pos$' : Copy(PosReal,'Pos$'),

    'Shares' : Copy(Nat,'Shares'),
    'PosShares' : Copy(PosInt,'PosShares'),

    '$/Shares': SApp('Rate', Copy(PosReal,'Pos$'), Copy(PosInt,'PosShares')),

    'Order' : Copy(SApp('Tuple', Nat, Nat),'Order'),
    'TDMap_Order': SApp('TDMap', Copy(SApp('Tuple',Nat,Nat),'Order')),

    '%' : "[0,1]"
}
def normalize_sort(s:Sort) -> Sort:
    if s in TEMP_SORT_IDENTIFICATION:
        return TEMP_SORT_IDENTIFICATION[s]
    else:
        return s



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

