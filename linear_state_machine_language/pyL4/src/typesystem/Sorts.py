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

    # 'TDMap_Order': NonatomicSort('TDMap', ('Order',)),
    'TDMap_Order': SApp('TDMap', SApp('Tuple',Nat,Nat)),

    '%' : "[0,1]"
}
def normalize_sort(s:Sort) -> Sort:
    if s in TEMP_SORT_IDENTIFICATION:
        return TEMP_SORT_IDENTIFICATION[s]
    else:
        return s

SortOps = ('TDMap','Rate','Tuple')

UnboundedNumericSorts = (Int,Nat,PosInt,Real,NonnegReal,PosReal)

BoundedNumericSorts = ("[0,1]","(0,1]","[0,1)","(0,1)")

# Because all non-empty intersections must be represented (for now)
FiniteNumericSorts = ("{0,1}","{0}","{1}")

AllAtomicSorts : Tuple[Any,...] = (DateTime,TimeDelta,PosTimeDelta,Bool) + BoundedNumericSorts + UnboundedNumericSorts + FiniteNumericSorts

# for
    # (NonatomicSort('Rate',Num,Denom), NonatomicSort('Rate',Num,Denom) for Num
# )

subtypes_data : Tuple[Tuple[Sort,...],...] = (
    (PosTimeDelta, TimeDelta),
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
    # TEMP. Willl generate these later.
    (SApp('Rate',PosInt, PosInt), SApp('Rate', PosInt, PosReal), SApp('Rate',PosReal,PosReal), SApp('Rate',NonnegReal,PosReal), SApp('Rate',Real,PosReal)),
    (SApp('Rate',Int, PosInt),    SApp('Rate', Int, PosReal),    SApp('Rate',Real,PosReal)),
    (SApp('Rate',Real, PosInt),    SApp('Rate', Real, PosReal)),
    (SApp('Rate',NonnegReal, PosInt),    SApp('Rate', NonnegReal, PosReal)),
    (SApp('Rate',NonnegReal, PosInt),    SApp('Rate', Real, PosInt)),
    (SApp('Rate',NonnegReal, PosReal), NonnegReal),
    (SApp('Rate',NonnegReal, PosInt), NonnegReal),
    (SApp('Rate',PosReal,PosReal), PosReal),
    (SApp('Rate',Real,PosReal), Real),
    (SApp('Rate',PosReal,PosInt), PosReal),
    (SApp('Rate',Real,PosInt), Real),

    (SApp('TDMap', SApp('Tuple',Nat,Nat)), SApp('TDMap', 'Any')),
    (SApp('TDMap', 'Any'), SApp('TDMap', SApp('Tuple',Nat,Nat))),
)

todo_once("temp computation of 'all sorts' (not really)")
NonatomicSortData : Tuple[Any,...] = (
    ('Rate',PosReal,PosInt), ('Rate',Real,PosInt), ('Rate',PosReal,PosInt),
    ('TDMap',('Tuple',PosInt, Nat)))

AllSortData : Tuple[Any,...] = AllAtomicSorts + (('Rate',PosReal,PosInt),)

AllSorts : Tuple[Sort,...] = AllAtomicSorts + tuple(TEMP_SORT_IDENTIFICATION.values())