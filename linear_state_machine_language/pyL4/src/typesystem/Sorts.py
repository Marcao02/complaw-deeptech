from typing import NamedTuple, Tuple, Any, Union, cast, NewType

SortOp = str # NewType('SortOp',str)
AtomicSort = str # NewType('AtomicSort',str)
Sort = Union[AtomicSort, 'NonatomicSort']
class NonatomicSort(NamedTuple):
    sortop: AtomicSort
    args_: Tuple[Any,...]
    @property
    def args(self) -> Tuple[Sort]:
        return cast(Tuple[Sort], self.args_)

DateTime = 'Datetime'
TimeDelta = 'Timedelta'
Int = 'Int'
PosInt = 'PosInt'
Nat = 'Nat'
Real = 'Real'
PosReal = 'PosReal'
NonnegReal = 'NonnegReal'
Bool = 'Bool'

SortOps = ('TDMap','Rate')

UnboundedNumericSorts = (Int,Nat,PosInt,Real,NonnegReal,PosReal)

BoundedNumericSorts = ("[0,1]","(0,1]","[0,1)","(0,0)")

# Because all non-empty intersections must be represented (for now)
FiniteNumericSorts = ("{0,1}","{0}","{1}")

AllAtomicSorts = (DateTime,TimeDelta,Bool) + BoundedNumericSorts + UnboundedNumericSorts

subtypes_data = (
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
    (Int,Real)
)
