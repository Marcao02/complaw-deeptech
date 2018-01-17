from enum import Enum
from typing import Any, List, Tuple, NamedTuple, Dict, Sequence, Optional, Union, NewType, Set, cast

from src.model.Literal import Literal, BoolLit, IntLit
from src.model.Section import Section
from src.model.Term import Term
from src.model.Action import Action
from src.model.ActionRule import NextActionRule
from src.model.L4Contract import L4Contract
from src.model.util import todo_once

todo_once("Copies of numeric types.")
NUM_NUMERIC_COPIES = 3

DT = 'Datetime'
TD = 'Timedelta'
Int = 'Int'
PosInt = 'PosInt'
Nat = 'Nat'
Real = 'Real'
PosReal = 'PosReal'
NonnegReal = 'NonnegReal'
Bool = 'Bool'

SortOps = ('TDMap','Tuple','Rate')

AllNumericTypes = (Int,Nat,PosInt,
                     Real,NonnegReal,PosReal)



AllPrimTypes = (DT,TD,Bool) + AllNumericTypes

SortOp = NewType('SortOp',str)
AtomicSort = str
class NonatomicSort(NamedTuple):
    sortop: SortOp
    args: Tuple[Any]
Sort = Union[AtomicSort, NonatomicSort]

class SimpleFnType(NamedTuple):
    parts: Sequence[Sort]

class OverloadedFnType(NamedTuple):
    parts: Sequence[SimpleFnType]

FnType = Union[SimpleFnType,OverloadedFnType]


def nested_list_replace(lst_or_str:Union[str,Sequence[Any]], old:str, new:str) -> Any:
    if isinstance(lst_or_str,str):
        return new if lst_or_str == old else lst_or_str
    else:
        return tuple(nested_list_replace(part, old, new) for part in lst_or_str)
def nested_list_replace_mult( lst_or_str:Union[str,Sequence[Any]],
                              subst:Dict[str,Any] ) -> Any:
    if isinstance(lst_or_str,str):
        return subst[lst_or_str] if lst_or_str in subst else lst_or_str
    else:
        return tuple(nested_list_replace_mult(part, subst) for part in lst_or_str)


def mafntype(dom:Any,ran:Any) -> Tuple[str,Any,Any]:
    return ("multi", dom, ran)

# simple function type
def sfntype(*tp:Any) -> Any:
    assert len(tp) >= 2
    return ('fn',) + tp

T = 'TVar'
N = 'NVar'
D = 'DVar'

def parametric(tp:Sequence[Any], substitutions:Sequence[Any] = AllPrimTypes, var:str = 'TVar') -> Sequence[Any]:
    # print(substitutions)
    return  [ nested_list_replace(tp, var, primtype) for primtype in substitutions ]

def parametric_mult(tp:Sequence[Any], substitutions:Sequence[Dict[str,Any]]) -> Sequence[Any]:
    # print(substitutions)
    return  [ nested_list_replace_mult(tp, subst) for subst in substitutions ]


# default_fn_template = lambda overloaded_symb: lambda tp: f'{overloaded_symb}{tp}'
subtypes_data = (
    [PosInt,Nat,Int],
    [PosReal,NonnegReal,Real],
    [PosInt,PosReal],
    [Nat,NonnegReal],
    [Int,Real]
)

subtypes_data_explicit : Dict[AtomicSort,Set[AtomicSort]] = cast(Dict[AtomicSort,Set[AtomicSort]], {
    'PosInt': {Nat,Int,NonnegReal,PosReal,Real},
    'Nat': {Int,NonnegReal,Real},
    'Int': {Real},
    'PosReal': {NonnegReal,Real},
    'NonnegReal': {Real},
    'Real': {}
})


NumericIntersection = Set[AtomicSort]
def simplifyNumericIntersectionUpdate(ni:NumericIntersection) -> None:
    for (sub,sups) in subtypes_data_explicit.items():
        if sub in ni:
            ni.difference_update(sups)
def simplifyNumericIntersection(ni:NumericIntersection) -> NumericIntersection:
    rv: NumericIntersection = ni
    for (sub,sups) in subtypes_data_explicit.items():
        if sub in rv:
            rv = rv.difference(sups)
    return rv

assert simplifyNumericIntersection({Real,PosInt}) == {PosInt}


overloaded_types_data : List[
                            Tuple[Sequence[str], Any] ] = [
    (('≤', '≥', '<', '>'), (
        mafntype(Real, Bool),
        mafntype(TD, Bool),
        mafntype(DT, Bool) )),
    (('==',),
     parametric(mafntype(T, Bool))),
    (('!=',),
     parametric(sfntype(T, T, Bool))),
    (('ifthenelse',),
     parametric(sfntype(Bool, T, T, T))),
    (('min','max','+','*') ,
        parametric(mafntype(T, T), AllNumericTypes + (TD,)) ),
    (('*',), sfntype(TD, Nat, TD)),
    (('*',), parametric_mult( sfntype(('Rate', N, D), D, N), (
                {'N':Real,'D':PosInt},
                {'N':PosReal, 'D': PosInt},
                {'N':NonnegReal, 'D': PosInt}
    ))),
    (('-',), parametric(sfntype(T, T, T), (Int, Real, TD))),
    (('/',), parametric(sfntype(T, T, T), (Real, PosReal, NonnegReal))),
    (('not',), sfntype(Bool, Bool)),
    (('and','or', 'and*'),
     mafntype(Bool, Bool)),
    (('floor','round','ceil'), (
        sfntype(Real, Int),
        sfntype(NonnegReal, Nat) )),
    (('ceil',), (
        sfntype(Real, Int),
        sfntype(PosReal, PosInt) )),
    (('even','odd'), parametric(sfntype(T, Bool), (Int, Nat, PosInt))),

    # this is not right cuz first arg type can be a superset of the other arg types.
    (('+=','*='), parametric(sfntype(T, T, T), AllNumericTypes))
]

# def construct_fntypes2() -> Dict[str, Union[SimpleFnType,OverloadedFnType]]:
#     fntypes_map : Dict[str, Union[SimpleFnType,OverloadedFnType]] = dict()
#     for pair in overloaded_types_data:
#         symbs : List[str] = [pair[0]] if isinstance(pair[0],str) else pair[0]
#         fntypes_data : List[List[str]] = [pair[1]] if (pair[1][0] == 'fn') else pair[1]
#         for symb in symbs:
#             if symb not in fntypes_map:
#                 fntypes_map[symb] = fntypes_data
#             else:
#                 fntypes_map[symb].extend(fntypes_data)
#     return fntypes_map


# def construct_fntypes() -> Dict[str, OverloadedFnType]:
#     fntypes_map : Dict[str, OverloadedFnType] = dict()
#     for pair in overloaded_types_data:
#         fst = pair[0]
#         snd : Any = pair[1]
#         symbs : Sequence[str] = cast(Sequence[str], (fst,) if isinstance(fst,str) else fst)
#         # fntypes : Sequence[SimpleFnType] = (SimpleFnType(snd[1:]),) if snd[0] == 'fn' else tuple(SimpleFnType(sftdata[1:]) for sftdata in snd)
#         fntypes : Sequence[Sequence[str]] = cast(Sequence[Sequence[str]], (pair[1],) if (pair[1][0] == 'fn') else pair[1])
#         for symb in symbs:
#             if symb not in fntypes_map:
#                 fntypes_map[symb] = OverloadedFnType(fntypes)
#             else:
#                 fntypes_map[symb].parts = fntypes_map[symb].parts + tuple(fntypes)
#     return fntypes_map

def construct_fntypes() -> Dict[str, Tuple[Sequence[Any],...]]:
    fntypes_map : Dict[str, Tuple[Sequence[Any],...]] = dict()
    for pair in overloaded_types_data:
        fst = pair[0]
        snd : Any = pair[1]
        symbs : Sequence[str] = cast(Sequence[str], (fst,) if isinstance(fst,str) else fst)
        # fntypes : Sequence[SimpleFnType] = (SimpleFnType(snd[1:]),) if snd[0] == 'fn' else tuple(SimpleFnType(sftdata[1:]) for sftdata in snd)
        fntypes : Tuple[Sequence[Any],...] = cast(Tuple[Sequence[Any],...], tuple((snd,) if (snd[0] == 'fn') else snd))
        for symb in symbs:
            if symb not in fntypes_map:
                fntypes_map[symb] = fntypes
            else:
                fntypes_map[symb] = fntypes_map[symb] + fntypes

    return fntypes_map

fntypes_map = construct_fntypes()
for symb in fntypes_map:
    print(symb)
    print("\t", str(fntypes_map[symb]))



class DataType(Enum):
    DT = 'Datetime'
    TD = 'Timedelta'
    Int = 'Int'
    Nat = 'Nat'
    Real = 'Real'
    Bool = 'Bool'


def typechecker(prog:L4Contract):
    def check(t:Term, sorts:List[str], a:Optional[Action], s:Optional[Section], rule:Optional[NextActionRule]):
        if isinstance(t, Literal):
            if isinstance(t, BoolLit):
                return Bool
            elif isinstance(t, IntLit):
                return Int

    def infer(t:Term, a:Optional[Action], s:Optional[Section], rule:Optional[NextActionRule]) :
        pass

    for section in prog.sections_iter():
        pass
    for action in prog.actions_iter():
        pass



# class SimpleFnType:
#     parts: List[DataType]
#
# class OverloadedFnType(NamedTuple):
#     types: List[SimpleFnType]
#
#     fntypes : Dict[str, SimpleFnType] = dict()
#
#     fnsymb_or_symbs : Union[str,List[str]]
#     fnsymbs : List[str]
#     types : Any
#     fntype_or_types_data: List[List[str]]
#     for [fn_symb_or_symbs, fntype_or_types_data] in overloaded_types_data:
#
#         fnsymbs = [fn_symb_or_symbs] if isinstance(fn_symb_or_symbs, str) else fnsymb_or_symbs
#
#         for fnsym in fnsymbs:
#
#             pass

# typechecker will only work on no-floating fragment.
# things to decide how to handle:
#   local vars (optional Action arg)
#   rule-bound action params (optional NextActionRule arg)
#   action-bound action params (optional Action arg)

# OverloadingTypeMap = Dict[str, List[Tuple[str, L4Type]]]
# # def eachof(*tp:Any) -> Any:
# #     return ['eachof'] + [x for x in tp]
#
# def multOfSameType(symbs:List[str], l4type:Any) -> Any:
#     return [[s, l4type] for s in symbs]
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


