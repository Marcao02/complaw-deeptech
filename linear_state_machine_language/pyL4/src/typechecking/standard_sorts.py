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
NormalUnboundedNumericSorts = {Int,Nat,PosInt,Real,NonnegReal,PosReal}

BoundedRealIntervalSorts = {"[0,1]", "(0,1]", "[0,1)", "(0,1)"}
# Because all non-empty intersections must be represented (for now)
FiniteNumericSorts = {"{0,1}","{0}","{1}"}
AtomicNumericSorts = NormalUnboundedNumericSorts\
                        .union(BoundedRealIntervalSorts)\
                        .union(FiniteNumericSorts)
                        # .union(NumericSortDups)

PositiveAtomicNumericSorts = {"{1}", "(0,1)", "(0,1]", PosInt, PosReal}

AllAtomicSorts : Set[Sort] = cast(Set[Sort], {DateTime, TimeDelta, PosTimeDelta, Bool, 'RoleId', 'EmptyTDMap'})\
                                .union(AtomicNumericSorts)


# ---------Nonatomic---------
NumericSortDups = cast(Set[Sort],{PosIntj,PosRealj,Natj,NonnegRealj} )
AllAtomicSortsAndDups : Set[Sort] = AllAtomicSorts.union(NumericSortDups)

RatioSorts : Set[Sort] = {Ratio(PosRealj, PosIntj), Ratio(NonnegRealj, PosIntj)}

TupleAtomicSorts : Set[Sort] = {SApp('Tuple',S,S) for S in AllAtomicSortsAndDups}

TDMapKeySorts = TupleAtomicSorts.union(AllAtomicSortsAndDups)
TDMapSorts = map(lambda t: SApp("TDMap", t), TDMapKeySorts)

AllSorts : Set[Any] = AllAtomicSortsAndDups\
                        .union( TupleAtomicSorts )\
                        .union( TDMapSorts ) \
                        .union( RatioSorts )

AtomicNumericSortsAndDups = NumericSortDups.union(cast(Set[Sort],AtomicNumericSorts))
UnboundedNumericSorts = cast(Set[Sort],NormalUnboundedNumericSorts).union( RatioSorts)
AllNumericSorts = UnboundedNumericSorts.union(BoundedRealIntervalSorts).union(FiniteNumericSorts)


def sort_compatible(s:Sort, U:Set[str], sort_defns:Dict[str,Sort]) -> bool:
    if isinstance(s,str):
        return sort_compatible(sort_defns[s],U,sort_defns) if s in sort_defns else s in U
    else:
        return sort_compatible(s.args[0],U,sort_defns) if s.sortop == "Dup" else False

def is_valid_sort(s:Sort, sort_defns:Dict[str,Sort]) -> bool:
    if isinstance(s,str):
        if s in sort_defns:
            return is_valid_sort(sort_defns[s], sort_defns)
        else:
            return s in AllAtomicSortsAndDups
    else:
        op = s.sortop
        if op == 'Ratio':
            numok =  sort_compatible(s.args[0], AtomicNumericSorts, sort_defns) #s.args[0] in AtomicNumericSorts or (isinstance(s.args[0], NonatomicSort) and s.args[0].sortop == 'Dup' and is_valid_sort(s.args[0].args[0]))
            denok = sort_compatible(s.args[1], PositiveAtomicNumericSorts, sort_defns) #s.args[1] in AtomicNumericSorts or (isinstance(s.args[1], NonatomicSort) and s.args[1].sortop == 'Dup' and is_valid_sort(s.args[1].args[0]))
            # print(f"??? ({numok},{denok}), {s}")
            return numok and denok
        elif op == 'Tuple':
            return all(arg in AllAtomicSortsAndDups for arg in s.args)
        elif op == 'TDMap':
            return s.args[0] in TDMapKeySorts
        elif op == 'Dup':
            return s.args[0] in {Nat,PosInt,NonnegReal,PosReal}
        raise NotImplementedError

def check_sorts_valid(sorts:Set[Sort], sort_defns:Dict[str,Sort]) -> None:
    for s in sorts:
        assert is_valid_sort(s,sort_defns), f"Explicitly-written sort {s} is not valid."