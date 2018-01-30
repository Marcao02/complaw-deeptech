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

def Dup(sort:Sort, name:str) -> NonatomicSort:
    return SApp('Dup',sort,name)

def Ratio(s1,s2):
    return SApp('Ratio',s1,s2)

# ---------Atomic---------
NormalUnboundedNumericSorts = cast(Set[Sort],{Int,Nat,PosInt,Real,NonnegReal,PosReal})
NumericSortDups = cast(Set[Sort],{PosIntj,PosRealj,Natj,NonnegRealj} )
BoundedRealIntervalSorts = {"[0,1]", "(0,1]", "[0,1)", "(0,1)"}
# Because all non-empty intersections must be represented (for now)
FiniteNumericSorts = {"{0,1}","{0}","{1}"}
AtomicNumericSorts = NormalUnboundedNumericSorts.union(NumericSortDups).union(BoundedRealIntervalSorts).union(FiniteNumericSorts)

PositiveNumericSorts = {"{1}","(0,1)","(0,1]",PosRealj,PosIntj}

AllAtomicSorts : Set[Sort] = cast(Set[Sort],{DateTime,TimeDelta,PosTimeDelta,Bool,'RoleId','EmptyTDMap'})\
                                .union(AtomicNumericSorts)

# ---------Nonatomic---------
RatioSorts : Set[Sort] = {Ratio(PosRealj, PosIntj), Ratio(NonnegRealj, PosIntj)}

TupleAtomicSorts : Set[Sort] = {SApp('Tuple',S,S) for S in AllAtomicSorts}

TDMapKeySorts = TupleAtomicSorts.union(AllAtomicSorts)
TDMapSorts = map(lambda t: SApp("TDMap", t), TDMapKeySorts)

AllSorts : Set[Any] = AllAtomicSorts\
                        .union( TupleAtomicSorts )\
                        .union( TDMapSorts ) \
                        .union( RatioSorts )

UnboundedNumericSorts = NormalUnboundedNumericSorts\
                            .union( NumericSortDups )\
                            .union( RatioSorts )

AllNumericSorts = UnboundedNumericSorts.union(BoundedRealIntervalSorts).union(FiniteNumericSorts)

def is_valid_sort(s:Sort, sort_defns:Dict[str,Sort]) -> bool:
    if isinstance(s,str):
        if s in sort_defns:
            return is_valid_sort(sort_defns[s], sort_defns)
        else:
            return s in AllAtomicSorts
    else:
        op = s.sortop
        if op == 'Ratio':
            return s.args[0] in AllNumericSorts and s.args[1] in PositiveNumericSorts

    return False