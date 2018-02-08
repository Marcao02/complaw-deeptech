from typing import FrozenSet

from src.independent.util_for_sets import fset
from src.independent.util import todo_once
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

def SApp(symb:str, *args:Any) -> SortOpApp:
    return SortOpApp.c(symb, tuple(args))

def Dimensioned(sort:Sort, name:str) -> SortOpApp:
    return SApp('Dimensioned',sort,name)

def Ratio(s1,s2):
    return SApp('Ratio',s1,s2)

unitvar1 = 'shares'
unitvar2 = '$'
# Will use for shares:
Nat1 = Dimensioned(Nat, unitvar1)
PosInt1 = Dimensioned(PosInt, unitvar1)
# Will use for money:
NonnegReal2 = Dimensioned(NonnegReal, unitvar2)
PosReal2 = Dimensioned(PosReal, unitvar2)

# Maybe these shouldn't exist, but they are useful for typing subtration
Int1 = Dimensioned(Int, unitvar1)
Real2 = Dimensioned(Real, unitvar2)


def fsortset(*args:Sort) -> FrozenSet[Sort]:
    return cast(FrozenSet[Sort], frozenset(args))

# ---------Atomic---------
NormalUnboundedNumericSorts = fsortset(Int,Nat,PosInt,Real,NonnegReal,PosReal)

BoundedRealIntervalSorts = fsortset("Fraction[0,1]", "Fraction(0,1]", "Fraction[0,1)", "Fraction(0,1)")
# Because all non-empty intersections must be represented (for now)
FiniteNumericSorts = fsortset("{0,1}","{0}","{1}")
AtomicNumericSorts = NormalUnboundedNumericSorts\
                        .union(BoundedRealIntervalSorts)\
                        .union(FiniteNumericSorts)
                        # .union(DimensionedNumericSorts)

PositiveAtomicNumericSorts = fsortset("{1}", "Fraction(0,1)", "Fraction(0,1]", PosInt, PosReal)

AllAtomicSorts = fsortset(DateTime, TimeDelta, PosTimeDelta, Bool, 'RoleId', 'EmptyTDMap','SortId')\
                  .union(AtomicNumericSorts)


# ---------Nonatomic---------

todo_once("SHOULD NOT BE PREGENERATING NONATOMIC SORTS\nNot because of efficiency, but because not knowing what the "
          "Dimensioned sorts are until a contract is read led me to implement a nasty hack that is now technical debt")

DimensionedNumericSorts = fsortset(Nat1, PosInt1, PosReal2, NonnegReal2, Real2)
AtomicSortsAndDimensionedNumericSorts = AllAtomicSorts.union(DimensionedNumericSorts)

# these are just the only ones we're currently using.
# later, will deduce which ones we're using in a contract, so they don't need to be declared like this.
RatioSorts = fsortset(Ratio(PosReal2, PosInt1), Ratio(NonnegReal2, PosInt1))

TupleAtomicAndDimensionedNumericSorts : FrozenSet[Sort] = frozenset(SApp('Tuple', S, S) for S in AtomicSortsAndDimensionedNumericSorts)

TDMapKeySorts = TupleAtomicAndDimensionedNumericSorts.union(AtomicSortsAndDimensionedNumericSorts)
TDMapSorts : FrozenSet[Sort] = frozenset(map(lambda t: SApp("TDMap", t), TDMapKeySorts))

AllSorts : FrozenSet[Sort] = TDMapKeySorts \
              .union( TDMapSorts ) \
              .union( RatioSorts )

AtomicNumericSortsAndDimensionedNumericSorts = DimensionedNumericSorts.union( AtomicNumericSorts )
UnboundedNumericSorts = NormalUnboundedNumericSorts.union( RatioSorts )
AllNumericSorts = UnboundedNumericSorts.union( BoundedRealIntervalSorts ).union(FiniteNumericSorts)



def check_sorts_valid(sorts:FrozenSet[Sort], sort_defns:Dict[str,Sort]) -> None:
    def sort_compatible(s: Sort, U: FrozenSet[Sort]) -> bool:
        if isinstance(s, str):
            return sort_compatible(sort_defns[s], U) if s in sort_defns else s in U
        else:
            return sort_compatible(s.args[0], U) if s.op == "Dimensioned" else False

    def is_valid_sort(s: Sort) -> bool:
        if isinstance(s, str):
            if s in sort_defns:
                return is_valid_sort(sort_defns[s])
            else:
                return s in AtomicSortsAndDimensionedNumericSorts
        else:
            op = s.op
            if op == 'Ratio':
                numok = sort_compatible(s.args[0], AtomicNumericSorts)
                denok = sort_compatible(s.args[1], PositiveAtomicNumericSorts)
                # print(f"??? ({numok},{denok}), {s}")
                return numok and denok
            elif op == 'Tuple':
                return all(arg in AtomicSortsAndDimensionedNumericSorts for arg in s.args)
            elif op == 'TDMap':
                return s.args[0] in TDMapKeySorts
            elif op == 'Dimensioned':
                return s.args[0] in {Nat, PosInt, NonnegReal, PosReal, Real}
            raise NotImplementedError

    for s in sorts:
        assert is_valid_sort(s), f"Explicitly-written sort {s} is not valid."