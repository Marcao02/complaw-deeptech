from typing import NamedTuple, Tuple, Any, Union, cast, NewType, Optional, Dict, Set

from src.util import todo_once, mapjoin

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
    def args(self) -> Tuple[Sort]:
        return cast(Tuple[Sort], self.args_)
    @staticmethod
    def mk(sortop:AtomicSort, args:Tuple[Sort,...]) -> 'NonatomicSort':
        return NonatomicSort(sortop, args)

    def __str__(self) -> str:
        if self.sortop == 'Tuple':
            return f"{mapjoin(str,self.args,'×')}"
        elif self.sortop == 'Rate':
            return f"{mapjoin(str,self.args,'/')}"
        else:
            return f"{self.sortop}[{mapjoin(str,self.args,', ')}]"
    def __repr__(self) -> str:
        return str(self)

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
TEMP_SORT_IDENTIFICATION : Dict[Sort,Sort] = {
    'ℚ' : Real,
    'ℕ' : Nat,
    '$' : NonnegReal,
    'Pos$' : PosReal,

    'Shares' : Nat,
    'PosShares' : PosInt,

    '$/Shares': SApp('Rate', PosReal, PosInt),

    'Order' : SApp('Tuple', Nat, Nat),
    'TDMap_Order': SApp('TDMap', SApp('Tuple',Nat,Nat)),

    '%' : "[0,1]"
}
def normalize_sort(s:Sort) -> Sort:
    if s in TEMP_SORT_IDENTIFICATION:
        return TEMP_SORT_IDENTIFICATION[s]
    else:
        return s

SortOps = {'TDMap','Rate','Tuple'}

UnboundedNumericSorts = {Int,Nat,PosInt,Real,NonnegReal,PosReal}

BoundedNumericSorts = {"[0,1]","(0,1]","[0,1)","(0,1)"}

# Because all non-empty intersections must be represented (for now)
FiniteNumericSorts = {"{0,1}","{0}","{1}"}

AllAtomicSorts : Set[str] = {DateTime,TimeDelta,PosTimeDelta,Bool}.union(BoundedNumericSorts).union(UnboundedNumericSorts).union(FiniteNumericSorts)

TupleAtomicSortData : Set[Any] = {('Tuple',S,S) for S in AllAtomicSorts}
TDMapKeySortData = TupleAtomicSortData.union(AllAtomicSorts)
AllSortData : Set[Any] = AllAtomicSorts.union(TupleAtomicSortData).union(TDMapKeySortData)

TupleAtomicSorts = map(SApp,TupleAtomicSortData)
TDMapKeySorts = map(SApp,TDMapKeySortData)
AllSorts : Set[Any] = cast(Set[Sort],AllAtomicSorts)\
                        .union( set(TEMP_SORT_IDENTIFICATION.values()) )\
                        .union( TupleAtomicSorts )\
                        .union( TDMapKeySorts )

