from typing import Union, Any, Iterator, cast

from src.model.BoundVar import StateTransformLocalVar, GlobalVar, ActionBoundActionParam, ContractParam
from src.model.GlobalStateTransformStatement import GlobalStateTransformStatement, StateTransformLocalVarDec, \
    GlobalVarAssignStatement
from src.model.L4Contract import L4Contract
from src.model.Literal import *
from src.model.Term import Term, FnApp, FnSymb
from src.typesystem.FnTypes import OverloadedFnType, SortTuple, SimpleFnType, ArbArityFnType, Optional
from src.typesystem.Sorts import Sort
from src.typesystem.StrictSubtypesGraph import StrictSubtypesGraph
from src.typesystem.standard_types import standard_types_graph
from src.util import todo_once

graph : StrictSubtypesGraph = standard_types_graph

class L4TypeError(Exception):
    pass

def sub(s1:Sort,s2:Sort) -> bool:
    if s1 == s2:
        return True
    if s1 == 'Any':
        print(f"warning: return true on check Any ⊆ {s2}")
        return True
    return graph.hasEdge(s1,s2)

def overloaded_fnapp_range_memo(oft:OverloadedFnType, argsorts:SortTuple, term: Optional[FnApp] = None) -> Optional[Sort]:
    todo_once("contrib: cast shouldn't be necessary")
    rangeset = set(cast(Iterator[Sort], filter(lambda x: x is not None, (ft_range(ft, argsorts) for ft in oft.parts))))
    if len(rangeset) == 0:
        msg = f"Domain of overloaded function type:\n{oft}\nis not a superset of arg sorts:\n{argsorts}"
        if not term:
            raise TypeError(msg)
        else:
            raise TypeError(msg + f"{msg} + \nTerm is {term}")
    try:
        intersection = graph.simplifyIntersection(rangeset)
    except Exception as e:
        print("Problem with overloaded function type:\n" + str(oft) + "\nand arg sorts:\n" + str(argsorts))
        raise(e)
    if intersection:
        oft.range_memo[argsorts] = intersection
        return intersection
    else:
        oft.illtyped_memo.add(argsorts)
        return None

def ft_range(fntype:Union[SimpleFnType,ArbArityFnType], argsorts:SortTuple) -> Optional[Sort]:
    rv = sft_range(fntype, argsorts) if isinstance(fntype, SimpleFnType) else aaft_range(fntype, argsorts)
    # print(f"arg sorts {argsorts} are not a subset of domain of simple fn type {fntype}")
    return rv

def sft_range(fntype:SimpleFnType, argsorts:SortTuple) -> Optional[Sort]:
    if len(fntype.parts) - 1 != len(argsorts):
        return None
    for i in range(len(argsorts)):
        argsort = argsorts[i]
        fnsort = fntype.parts[i]
        if not sub(argsort,fnsort):
            return None
    return fntype.ran

def aaft_range(fntype:ArbArityFnType, argsorts:SortTuple) -> Optional[Sort]:
    for i in range(len(argsorts)):
        argsort = argsorts[i]
        if not sub(argsort,fntype.dom):
            return None
    return fntype.ran


def typeinfer_term(t:Term) -> Sort:
    if isinstance(t,FnApp):
        fnsymb: FnSymb = t.fnsymb
        if fnsymb.name == 'cast':
            return cast(SortLit,t.args[0]).lit

        argsorts = tuple(typeinfer_term(arg) for arg in t.args)

        if fnsymb.type:
            try:
                rv = overloaded_fnapp_range_memo(fnsymb.type, argsorts, t)
            except Exception as e:
                print("Problem with inferring sort of term\n:" + str(t))
                raise e
            if rv is None:
                raise TypeError(f"Term {t}\nArg sorts {argsorts}\nFn type:\n{fnsymb.type}")
            return rv
        else:
            raise TypeError(t, f"Function symbol {fnsymb} has no type")
    elif isinstance(t,Literal):
        if isinstance(t,IntLit):
            #
            # if t.lit == 0:
            #     return "{0}"
            # elif t.lit == 1:
            #     return "{1}"
            if t.lit > 0:
                return "PosInt"
            elif t.lit == 0:
                return "Nat"
            else:
                return "Int"
        elif isinstance(t,FloatLit):
            return "Real"
        elif isinstance(t,BoolLit):
            return "Bool"
        elif isinstance(t,SimpleTimeDeltaLit):
            return "TimeDelta"
        elif isinstance(t,DeadlineLit):
            todo_once('type for deadline literals? or ensure this never comes up?')
            return "DeadlineLit"
        elif isinstance(t,RoleIdLit):
            return "RoleId"
        elif isinstance(t,StringLit):
            return "String"
    elif isinstance(t, (StateTransformLocalVar, GlobalVar)):
        return t.vardec.sort
    elif isinstance(t, ActionBoundActionParam):
        # print(t.action.param_types)
        return t.action.param_types[t.name]
    elif isinstance(t,ContractParam):
        return t.paramdec.sort
    raise Exception(f"typeinfer_term unhandled case for {type(t)}: {t}" )

def typecheck(x:Any):
    if isinstance(x,L4Contract):
        typecheck_prog(x)
    elif isinstance(x,Term):
        typeinfer_term(x)
    elif isinstance(x,GlobalStateTransformStatement):
        typecheck_statement(x)

def typecheck_term(t:Term, s:Sort) -> bool:
    inferred = typeinfer_term(t)
    assert inferred is not None
    return graph.hasEdge(inferred,s)


def typecheck_statement(s:GlobalStateTransformStatement):
    if isinstance(s, StateTransformLocalVarDec):
        typecheck_term(s.value_expr,s.sort)
    elif isinstance(s, GlobalVarAssignStatement):
        typecheck_term(s.value_expr,s.vardec.sort)

def typecheck_prog(prog:L4Contract):
    for action in prog.actions_iter():
        msg2 = f"Typechecking Action {action.action_id}"
        print( "-"*len(msg2) + "\n" + msg2)
        for statement in action.state_transform_statements():
            typecheck_statement(statement)