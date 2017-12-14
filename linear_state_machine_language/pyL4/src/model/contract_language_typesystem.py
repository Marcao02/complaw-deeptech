from typing import Any, List

dt = 'datetime'
td = 'timedelta'
Int = 'int'
Nat = 'nat'
Real = 'real'
Bool = 'bool'

L4Type = any

def oneOrMore(tp:Any) -> Any:
    return ['oneOrMore',tp]

def fn(*tp:Any) -> Any:
    return ['fn'] + [x for x in tp]

# def eachof(*tp:Any) -> Any:
#     return ['eachof'] + [x for x in tp]

def multOfSameType(symbs:List[str], l4type:Any) -> Any:
    return [[s, l4type] for s in symbs]

types = [['days', fn(Int,td)],
    ['not', fn(Bool,Bool)] ] + \
    multOfSameType(['and','or'], fn(oneOrMore(Bool), Bool)) + \
    multOfSameType(['minInt','maxInt'], fn(oneOrMore(Int),Int)) + \
    multOfSameType(['minNat','maxNat'], fn(oneOrMore(Nat),Nat)) + \
    multOfSameType(['minReal','maxReal'], fn(oneOrMore(Real),Real)) + \
    multOfSameType(['ceil','floor','round'], fn(Real,Int) ) + \
    multOfSameType(['evenInt','oddInt'], fn(Int,Bool)) + \
    multOfSameType(['evenNat','oddNat'], fn(Nat,Bool))