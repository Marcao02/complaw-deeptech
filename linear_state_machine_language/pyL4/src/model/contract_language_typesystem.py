from enum import Enum
from typing import Any, List, Tuple, NamedTuple, Dict

DT = 'Datetime'
TD = 'Timedelta'
Int = 'Int'
Nat = 'Nat'
Real = 'Real'
Bool = 'Bool'
AllPrimTypes = [DT,TD,Int,Nat,Real,Bool]

L4Type = Any

def oneOrMore(tp:Any) -> Any:
    return ['oneOrMore',tp]

def fntype(*tp:Any) -> Any:
    return ['fn'] + [x for x in tp]

# def eachof(*tp:Any) -> Any:
#     return ['eachof'] + [x for x in tp]

def multOfSameType(symbs:List[str], l4type:Any) -> Any:
    return [[s, l4type] for s in symbs]

OverloadingTypeMap = Dict[str, List[Tuple[str, L4Type]]]

default_fn_template = lambda overloaded_symb: lambda tp: f'{overloaded_symb}{tp}'

T = 'TVar'
overloaded_types_data = [
    [['==','<','≤','>','≥',], [fntype(oneOrMore(T), Bool), AllPrimTypes]],
    ['!=', [fntype(T, T, Bool), AllPrimTypes]],
    [['min','max','+','-'] , [ fntype(oneOrMore(T),T), [Int,Nat,Real,TD]]],
    # [['min','max'] , [fntype(oneOrMore(T), T), [Int, Nat, Real, TD, DT]]],
    ['not', [fntype(Bool, Bool)]],
    [['ceil','floor','round'], fntype(Real, Int)],
    [['even','odd'], [fntype(T, Bool), [Int, Nat]]],
    [['ifthenelse'], [fntype(Bool, T, T, T), AllPrimTypes]],
    [['+=','*='], [fntype(T, T, T), [Int, Nat, Real, TD]]]
]

class DataType(Enum):
    DT = 'Datetime'
    TD = 'Timedelta'
    Int = 'Int'
    Nat = 'Nat'
    Real = 'Real'
    Bool = 'Bool'

class SimpleFnType:
    parts: List[DataType]

class OverloadedFnType(NamedTuple):
    types: List[SimpleFnType]



# simple_types = [ ['days', fn(Int,td)],
#           ['not', fn(Bool,Bool)],
#           ['==Int', fn(oneOrMore(Int), Bool)],
#           ['==Nat', fn(oneOrMore(Nat), Bool)],
#           ['==Real', fn(oneOrMore(Real), Bool)]
#         ] + \
#     multOfSameType(['and','or'], fn(oneOrMore(Bool), Bool)) + \
#     multOfSameType(['minInt','maxInt'], fn(oneOrMore(Int),Int)) + \
#     multOfSameType(['minNat','maxNat'], fn(oneOrMore(Nat),Nat)) + \
#     multOfSameType(['minReal','maxReal'], fn(oneOrMore(Real),Real)) + \
#     multOfSameType(['ceil','floor','round'], fn(Real,Int) ) + \
#     multOfSameType(['evenInt','oddInt'], fn(Int,Bool)) + \
#     multOfSameType(['evenNat','oddNat'], fn(Nat,Bool)) + \
#     multOfSameType(['+Nat','*Nat','-Nat','/Nat'], fn(oneOrMore(Nat),Nat)) + \
#     multOfSameType(['+Int','*Int','-Int','/Int'], fn(oneOrMore(Int),Int)) + \
#     multOfSameType(['+Real','*Real','-Real','/Real'], fn(oneOrMore(Real),Real)) + \
#     multOfSameType(['+=Nat','-=Nat','*=Nat'], fn([Nat,Nat],Nat)) + \
#     multOfSameType(['+=Int','-=Int','*=Int'], fn([Nat,Nat],Nat))


