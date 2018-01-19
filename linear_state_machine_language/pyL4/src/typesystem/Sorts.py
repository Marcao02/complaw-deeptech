from typing import NamedTuple, Tuple, Any, Union, cast, NewType, Optional, Dict

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
    def mk(sortop:AtomicSort, args:Tuple[Sort]) -> 'NonatomicSort':
        return NonatomicSort(sortop, args)

    def __str__(self) -> str:
        return f"{self.sortop}({mapjoin(str,self.args,', ')})"
    def __repr__(self) -> str:
        return str(self)

DateTime = 'DateTime'
TimeDelta = 'TimeDelta'
Int = 'Int'
PosInt = 'PosInt'
Nat = 'Nat'
Real = 'Real'
PosReal = 'PosReal'
NonnegReal = 'NonnegReal'
Bool = 'Bool'

todo_once("this is temporary too")
TEMP_SORT_IDENTIFICATION : Dict[Sort,Sort] = {
    'ℚ' : Real,
    'ℕ' : Nat,
    '$' : NonnegReal,
    'Pos$' : PosReal,

    'Shares' : Nat,
    'PosShares' : PosInt,

    '$/Shares': NonatomicSort('Rate', (PosReal, PosInt)),

    '%' : "[0,1]"
}
def normalize_sort(s:Sort):
    if s in TEMP_SORT_IDENTIFICATION:
        return TEMP_SORT_IDENTIFICATION[s]
    else:
        return s

SortOps = ('TDMap','Rate','Tuple')

UnboundedNumericSorts = (Int,Nat,PosInt,Real,NonnegReal,PosReal)

BoundedNumericSorts = ("[0,1]","(0,1]","[0,1)","(0,0)")

# Because all non-empty intersections must be represented (for now)
FiniteNumericSorts = ("{0,1}","{0}","{1}")

AllAtomicSorts : Tuple[Any,...] = (DateTime,TimeDelta,Bool) + BoundedNumericSorts + UnboundedNumericSorts + FiniteNumericSorts

subtypes_data : Tuple[Tuple[Sort,...],...] = (
    ("{0}","[0,1)"),
    ("{1}","(0,1]"),
    ("{0,1}","[0,1]"),
    ("{0}","{0,1}",Nat),
    ("{1}",PosInt),
    ("(0,1)", "[0,1)", "[0,1]", NonnegReal),
    ("(0,1)", "(0,1]", "[0,1]", NonnegReal),
    ("(0,1)", "(0,1]", PosReal),
    (PosInt,Nat,Int),
    (PosReal,NonnegReal,Real),
    (PosInt,PosReal),
    (Nat,NonnegReal),
    (Int,Real),
    (NonatomicSort('Rate', (PosInt, PosInt)), NonatomicSort('Rate', (PosInt, PosReal)), NonatomicSort('Rate',(PosReal,PosReal)), NonatomicSort('Rate',(Real,PosReal))),
    (NonatomicSort('Rate', (Int, PosInt)), NonatomicSort('Rate', (Int, PosReal)), NonatomicSort('Rate',(Real,PosReal))),
    (NonatomicSort('Rate',(PosReal,PosReal)), PosReal),
    (NonatomicSort('Rate',(Real,PosReal)), Real),
    (NonatomicSort('Rate',(PosReal,PosInt)), PosReal),
    (NonatomicSort('Rate',(Real,PosInt)), Real)
)

todo_once("temp computation of 'all sorts' (not really)")
NonatomicSortData : Tuple[Any,...] = (
    ('Rate',PosReal,PosInt), ('Rate',Real,PosInt), ('Rate',PosReal,PosInt),
    ('TDMap',('Tuple',PosInt, Nat)))

AllSortData : Tuple[Any,...] = AllAtomicSorts + (('Rate',PosReal,PosInt),)

AllSorts : Tuple[Sort,...] = AllAtomicSorts + (NonatomicSort.c('Rate',(PosReal,PosInt)),)