from typing import Union, Iterator, cast

from src.model.FnTypes import OverloadedFnType, SortTuple, SimpleFnType, ArbArityFnType
from src.model.Action import Action
from src.model.ActionRule import ActionRule
from src.model.BoundVar import StateTransformLocalVar, GlobalVar, ActionBoundActionParam, ContractParam, \
    RuleBoundActionParam
from src.model.FnSymb import FnSymb
from src.model.GlobalStateTransformStatement import GlobalStateTransformStatement, StateTransformLocalVarDec, \
    GlobalVarAssignStatement
from src.model.L4Contract import L4Contract
from src.model.Literal import *
from src.model.Section import Section
from src.model.Term import FnApp
from src.typesystem.L4TypeErrors import *
from src.typesystem.standard_subtype_graph import standard_types_graph
from src.util import todo_once

graph = standard_types_graph
def sub(s1:Sort,s2:Sort) -> bool:
    return graph.hasEdge(s1,s2)

def overloaded_fnapp_range_memo(oft:OverloadedFnType, argsorts:SortTuple, term: FnApp) -> Optional[Sort]:
    todo_once("contrib: cast shouldn't be necessary")
    rangeset = set(cast(Iterator[Sort], filter(lambda x: x is not None, (ft_range(ft, argsorts,term) for ft in oft.parts))))
    if len(rangeset) == 0:
        msg = f"Domain of overloaded function type:\n{oft}\nis not a superset of arg sorts:\n{argsorts}"
        raise L4TypeInferError(term, msg)
    try:
        intersection = graph.simplifyIntersection(rangeset)
    except Exception as e:
        print("Problem with overloaded function type:\n" + str(oft) + "\nand arg sorts:\n" + str(argsorts))
        raise e
    if intersection:
        oft.range_memo[argsorts] = intersection
        return intersection
    else:
        oft.illtyped_memo.add(argsorts)
        return None

def ft_range(fntype:Union[SimpleFnType,ArbArityFnType], argsorts:SortTuple, term: FnApp) -> Optional[Sort]:
    rv = sft_range(fntype, argsorts, term) if isinstance(fntype, SimpleFnType) else aaft_range(fntype, argsorts, term)
    # if not rv:
    #     print(f"arg sorts {argsorts} of {term.head} are NOT a subset of domain of nonoverloaded fn type {fntype}")
    # else:
    #     print(f"arg sorts {argsorts} of {term.head} ARE a subset of domain of nonoverloaded fn type {fntype}")
    return rv

def sft_range(fntype:SimpleFnType, argsorts:SortTuple, term: FnApp) -> Optional[Sort]:
    if len(fntype.parts) - 1 != len(argsorts):
        return None
    for i in range(len(argsorts)):
        argsort = argsorts[i]
        fnsort = fntype.parts[i]
        if not sub(argsort,fnsort):
            return None
    # print(f"Range of fntype {fntype} is {fntype.ran}")
    return fntype.ran

def aaft_range(fntype:ArbArityFnType, argsorts:SortTuple, term: FnApp) -> Optional[Sort]:
    for i in range(len(argsorts)):
        argsort = argsorts[i]
        if not sub(argsort,fntype.dom):
            return None
    return fntype.ran


"""
Just a public API function
"""
# def typecheck(x: Any):
#     if isinstance(x, L4Contract):
#         typecheck_prog(x)
#     elif isinstance(x, Term):
#         typeinfer_term(x)
#     elif isinstance(x, GlobalStateTransformStatement):
#         typecheck_statement(x)

def typecheck_prog(prog:L4Contract):
    tc = TypeChecker(prog)
    for action in prog.actions_iter():
        tc.typecheck_action(action)
    for section in prog.sections_iter():
        tc.typecheck_section(section)

class TypeChecker:
    def __init__(self,prog:L4Contract) -> None:
        self.prog = prog

    def typeinfer_term(self, t:Term) -> Sort:
        if isinstance(t,FnApp):
            fnsymb: FnSymb = t.fnsymb
            if fnsymb.name == 'cast':
                return cast(Sort, cast(SortLit,t.args[0]).lit)

            argsorts = tuple(self.typeinfer_term(arg) for arg in t.args)

            if fnsymb.type:
                try:
                    rv = overloaded_fnapp_range_memo(fnsymb.type, argsorts, t)
                except Exception as e:
                    print("Problem with inferring sort of term:\n" + str(t))
                    print("The original exception: ", e)
                    raise e
                if rv is None:
                    raise L4TypeInferError(t, f"overloaded_fnapp_range_memo return None.\nArg sorts: {argsorts}\nFn type:\n{fnsymb.type}")
                return rv
            else:
                raise L4TypeInferError(t, f"Function symbol {fnsymb} has no type.")
        elif isinstance(t,Literal):
            if isinstance(t,IntLit):
                if t.lit == 0:
                    return "{0}"
                elif t.lit == 1:
                    return "{1}"
                elif t.lit > 0:
                    return "PosInt"
                else:
                    return "Int"
            elif isinstance(t,FloatLit):
                return "Real"
            elif isinstance(t,BoolLit):
                return "Bool"
            elif isinstance(t,SimpleTimeDeltaLit):
                if t.num > 0:
                    return "PosTimeDelta"
                else:
                    return "TimeDelta"
            elif isinstance(t,DeadlineLit):
                todo_once('type for deadline literals? or ensure this never comes up?')
                return "Bool"
            elif isinstance(t,RoleIdLit):
                return "RoleId"
            elif isinstance(t,StringLit):
                return "String"
        elif isinstance(t, (StateTransformLocalVar, GlobalVar)):
            return t.vardec.sort
        elif isinstance(t, ActionBoundActionParam):
            # print(t.action.param_sorts_by_name)
            return t.action.param_sorts_by_name[t.name]
        elif isinstance(t,ContractParam):
            return t.paramdec.sort
        elif isinstance(t,RuleBoundActionParam):
            action = self.prog.action(t.action_rule.action_id)
            return action.param_sort(t.ind)


        raise Exception(f"typeinfer_term unhandled case for {type(t)}: {t}" )

    def typecheck_term(self, t:Term, s:Sort) -> bool:
        inferred = self.typeinfer_term(t)
        assert inferred is not None
        if not sub(inferred,s):
            print(f"{inferred} not ≤ {s}")
            raise L4TypeInferCheckError(t,inferred,s)
        return True

    def typecheck_statement(self, s:GlobalStateTransformStatement):
        if isinstance(s, StateTransformLocalVarDec):
            self.typecheck_term(s.value_expr,s.sort)
        elif isinstance(s, GlobalVarAssignStatement):
            self.typecheck_term(s.value_expr,s.vardec.sort)

    def typecheck_action_rule(self, rule:ActionRule):
        action = self.prog.action(rule.action_id)
        if rule.entrance_enabled_guard:
            self.typecheck_term(rule.entrance_enabled_guard, 'Bool')
        if rule.where_clause:
            self.typecheck_term(rule.where_clause, 'Bool')
        if rule.fixed_args:
            for i in range(len(rule.fixed_args)):
                argterm = rule.fixed_args[i]
                paramname = action.param_names[i]
                paramsort = action.param_sorts_by_name[paramname]
                self.typecheck_term(argterm, paramsort)
        if rule.time_constraint:
            self.typecheck_term(rule.time_constraint, 'Bool')


    def typecheck_section(self, section:Section):
        for rule in section.action_rules():
            self.typecheck_action_rule(rule)

    def typecheck_action(self, action:Action):
        msg2 = f"Typechecking Action {action.action_id}"
        print("-" * len(msg2) + "\n" + msg2)
        for statement in action.state_transform_statements():
            self.typecheck_statement(statement)
        for rule in action.future_action_rules():
            assert isinstance(rule,ActionRule)
            self.typecheck_action_rule(rule)

