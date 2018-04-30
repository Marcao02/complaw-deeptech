from src.independent.TransitivelyClosedDirectedGraph import TransitivelyClosedDirectedGraphInvariantError
from src.independent.util import todo_once
from src.model.Action import Action
from src.model.ActionRule import ActionRule
from src.model.BoundVar import LocalVar, StateVar, ActionBoundActionParam, ContractParam, \
    RuleBoundActionParam, PrimedStateVar
from src.model.FnTypes import OverloadedFnType, SortTuple, SimpleFnType, SimpleFnType
from src.model.Statement import Statement, LocalVarDec, \
    StateVarAssign, IfElse, FVRequirement
from src.model.L4Contract import L4Contract
from src.model.Literal import *
from src.model.Situation import Situation
from src.model.Sort import SortOpApp
from src.model.Term import FnApp
from src.typechecking.L4TypeErrors import *
from src.typechecking.standard_function_types import STANDARD_FNTYPES, print_types_map, ASSOCIATIVE_OPS, CHAIN_PREDS, \
    FnTypesMap
from src.typechecking.standard_subtype_graph import STANDARD_SUBSORTING_GRAPH, NormalUnboundedNumericSorts, \
    SubsortGraph, duplicate_some_edges
from src.typechecking.standard_sorts import check_sorts_valid

# print_types_map(STANDARD_FNTYPES)

def sub(s1:Sort,s2:Sort) -> bool:
    if isinstance(s2,SortOpApp):
        if isinstance(s1, SortOpApp):
            if 'Tuple' == s1.op == s2.op and len(s1) == len(s2):
                return all(sub(s1.args[i],s2.args[i]) for i in range(len(s1)))
            elif 'TDMap' == s1.op == s2.op:
                return sub(s1.args[0], s2.args[0])
        elif s2.op == 'TDMap':
            return s1 == 'EmptyTDMap'

    return STANDARD_SUBSORTING_GRAPH.hasEdge(s1, s2)

"""
Just a public API function
"""
# def typecheck(x: Any):
#     if isinstance(x, L4Contract):
#         typecheck_prog(x)
#     elif isinstance(x, Term):
#         typeinfer_term(x)
#     elif isinstance(x, Statement):
#         typecheck_statement(x)



def what_sorts_used_explicitly_or_at_leaves(prog:L4Contract) -> Iterable[Sort]:
    def f(t:Term):
        if isinstance(t, Literal):
            if isinstance(t, IntLit):
                if t.lit == 0:
                    yield "{0}"
                elif t.lit == 1:
                    yield "{1}"
                elif t.lit > 0:
                    yield "PosInt"
                else:
                    yield "Int"
            elif isinstance(t, FloatLit):
                if t.lit == 0:
                    yield "{0}"
                elif t.lit == 1:
                    yield "{1}"
                elif 0 < t.lit < 1:
                    yield "Fraction(0,1)"
                elif t.lit > 1:
                    yield "PosReal"
                else:
                    yield "Real"
            elif isinstance(t, BoolLit):
                yield "Bool"
            elif isinstance(t, SimpleTimeDeltaLit):
                if t.num > 0:
                    yield "PosTimeDelta"
                else:
                    yield "TimeDelta"
            elif isinstance(t, TimeDeltaLit):
                if t.lit.total_seconds() > 0:
                    yield "PosTimeDelta"
                else:
                    yield "TimeDelta"
            elif isinstance(t, RoleIdLit):
                yield "RoleId"
            elif isinstance(t, StringLit):
                yield "String"
            elif isinstance(t, SortLit):
                yield t.lit
            elif isinstance(t, DateTimeLit):
                return "DateTime"
            else:
                raise NotImplementedError(str(t) + "," + str(type(t)))
        elif isinstance(t, (LocalVar, StateVar)):
            yield t.vardec.sort
        elif isinstance(t, ActionBoundActionParam):
            yield t.action.param_sorts_by_name[t.name]
        elif isinstance(t, ContractParam):
            yield t.paramdec.sort
        elif isinstance(t, RuleBoundActionParam):
            action = prog.action(t.action_rule.action_id)
            yield action.param_sort(t.ind)

    return prog.forEachTerm(f)

def what_sorts_used_explicitly(prog:L4Contract) -> Iterable[Sort]:
    def f(t:Term):
        if isinstance(t, SortLit):
            yield t.lit
        elif isinstance(t, (LocalVar, StateVar)):
            yield t.vardec.sort
        elif isinstance(t, ActionBoundActionParam):
            yield t.action.param_sorts_by_name[t.name]
        elif isinstance(t, ContractParam):
            yield t.paramdec.sort
        elif isinstance(t, RuleBoundActionParam):
            action = prog.action(t.action_rule.action_id)
            yield action.param_sort(t.ind)

    return prog.forEachTerm(f)

def what_fnsymbols_used(prog:L4Contract) -> Iterable[str]:
    def f(t:Term):
        if isinstance(t, FnApp):
            yield t.fnsymb_name
    return prog.forEachTerm(f)

def what_fnsymbols_used2(prog:L4Contract) -> Iterable[str]:
    pred = lambda t: isinstance(t,FnApp)
    def f(t:FnApp):
        # if isinstance(t, FnApp):
        yield t.fnsymb_name
    return prog.forEach(pred,f)

def what_fnsymbol_arity_pairs_used(prog:L4Contract) -> Iterable[Tuple[str,int]]:
    pred = lambda t: isinstance(t,FnApp)
    def f(t:FnApp):
        # if isinstance(t, FnApp):
        yield (t.fnsymb_name, len(t.args))
    return prog.forEach(pred,f)



# def addDimensionedNumericSortRelnsToGraphAndFnTypes(sorts:Iterable[Sort], expanded_sortdefns: Dict[str, Sort], graph:SubsortGraph, fntypes:FnTypesMap):
#     substForVar: Dict[Sort, Sort] = {}
#
#     unit_vars_mentioned : Dict[Sort,List[str]] = {[] for s in sorts}
#     for s in sorts:
#
#
#     for s in sorts:
#         if isinstance(s,str) and s in expanded_sortdefns:
#             s = expanded_sortdefns[s]
#         if isinstance(s, SortOpApp) and s.op == "Dimensioned":
#             withvar = SortOpApp.c("Dimensioned", (s.args[0], dupvar))
#             substForVar[withvar] = s
#             # substForVar[s] = withvar
#             graph.addNode(s)
#             graph.addEdge(withvar, s)
#     for fsymb, oft in fntypes.items():
#         oft.add_substdict_copies(substForVar)
#         # oft.replace_sorts(substForVar)
#
#     print_types_map(fntypes)
#
#     duplicate_some_edges(substForVar, graph)

def typecheck_prog(prog:L4Contract, verbose=True):
    def maybesay(*args:Any):
        if verbose:
            print(args)

    sorts_used_explicitly = frozenset(what_sorts_used_explicitly(prog)).union(prog.sort_definitions.keys())
    maybesay(f"Explicit sorts:\n", sorts_used_explicitly)
    if verbose:
        print(f"Explicit sorts:\n", sorts_used_explicitly)

    literal_sorts = frozenset(what_sorts_used_explicitly_or_at_leaves(prog))
    if verbose:
        print(f"Nonexplicit sorts of literals:\n",literal_sorts.difference(sorts_used_explicitly))

    check_sorts_valid(sorts_used_explicitly, prog.sort_definitions)
    # addDimensionedNumericSortRelnsToGraphAndFnTypes(sorts_used_explicitly, prog.expanded_sort_definitions, STANDARD_SUBSORTING_GRAPH, STANDARD_FNTYPES)

    fsymbs = set(what_fnsymbols_used(prog))
    if verbose:
        print(f"Explicit fn symbols:\n", fsymbs)
    assert fsymbs == set(what_fnsymbols_used2(prog))

    tc = TypeChecker(prog, verbose=verbose)

    for action in prog.actions_iter():
        tc.typecheck_action(action, verbose=verbose)

    for situation in prog.situations_iter():
        tc.typecheck_situation(situation, verbose=verbose)

    if verbose:
        msg2 = f"Typechecking contract param declarations"
        print(msg2 + "\n" + "-" * len(msg2))
    for contract_param_dec in prog.contract_params.values():
        if contract_param_dec.value_expr:
            tc.typecheck_term(contract_param_dec.value_expr, contract_param_dec.sort)

    if verbose:
        msg2 = f"Typechecking state var declarations"
        print(msg2 + "\n" + "-" * len(msg2))
    for gvardec in prog.state_var_decs.values():
        if gvardec.initval:
            tc.typecheck_term(gvardec.initval, gvardec.sort)

    if verbose:
        print("✓\n")

def sort_expanded_display(sort:Sort, expanded:Sort) -> str:
    if sort == expanded:
        return str(sort)
    else:
        return f"{expanded} ({sort})"


class TypeChecker:
    def __init__(self, prog:L4Contract, verbose=True) -> None:
        self.prog = prog
        self.verbose = verbose

    def replace_sort_def(self, sort:Sort) -> Sort:
        # if sort == "$":
        #     print("hmmmmm", sort, self.prog.sort_definitions)
        if isinstance(sort,str) and sort in self.prog.sort_definitions:
            # return self.prog.sort_definitions[sort]
            assert sort in self.prog.expanded_sort_definitions
            return self.prog.expanded_sort_definitions[sort]
        else:
            return sort

    def sort_tuple_toStr(self, x:SortTuple) -> str:
        return " × ".join(map(str, map(self.replace_sort_def, x)))

    def overloaded_fnapp_range_memo(self, oft: OverloadedFnType, argsorts: SortTuple, term: FnApp) -> Optional[Sort]:
        if argsorts in oft.range_memo:
            return oft.range_memo[argsorts]

        rangeset = set(
            cast(Iterator[Sort], filter(lambda x: x is not None, (self.sft_range(ft, argsorts) for ft in oft.parts))))
        if len(rangeset) == 0:
            msg = f"\nDomain of this overloaded function type:\n{oft}\nis not a superset of arg sorts:\n{self.sort_tuple_toStr(argsorts)}"
            raise L4TypeInferError(term, msg)
        try:
            intersection = STANDARD_SUBSORTING_GRAPH.simplifyIntersection(rangeset, sub)
        except Exception as e:
            print("\nProblem with overloaded function type:\n" + str(oft) + "\nand arg sorts:\n" + str(argsorts))
            raise e

        oft.range_memo[argsorts] = intersection
        return intersection # possibly None


    def sft_range(self, fntype: SimpleFnType, argsorts: SortTuple) -> Optional[Sort]:
        if len(fntype.parts) - 1 != len(argsorts):
            return None
        for i in range(len(argsorts)):
            argsort = self.replace_sort_def(argsorts[i])
            fnsort = fntype.parts[i]
            if not sub(argsort, fnsort):
                return None
        # print(f"Range of fntype {fntype} is {fntype.ran}")
        # if not rv:
        #     print(f"arg sorts {argsorts} of {term.head} are NOT a subset of domain of nonoverloaded fn type {fntype}")
        # else:
        #     print(f"arg sorts {argsorts} of {term.head} ARE a subset of domain of nonoverloaded fn type {fntype}")
        return fntype.ran

    def cast_allowed(self, t:Term, sfrom:Sort, sto:Sort) -> bool:
        if isinstance(t,Literal) and isinstance(sto,SortOpApp) and sto.op == "Dimensioned" and isinstance(sfrom,str):
            return sub( sfrom, sto.args[0] )
        else:
            return sub( sto, sfrom )


    def typeinfer_term_then_sortdef_subst(self, t:Term) -> Sort:
        return self.replace_sort_def(self.typeinfer_term(t))


    def typeinfer_term(self, t:Term) -> Sort:
        if isinstance(t,FnApp):
            # ------------`cast` (former name for `check`), `trust`, and `units` ------------
            if t.fnsymb_name in {'cast','check','trust','units'}:
                assert len(t.args) == 2
                casted_term = t.args[1]
                sortlit = t.args[0]
                assert isinstance(sortlit, SortLit)
                casted_to = sortlit.lit
                casted_to_long = self.replace_sort_def(casted_to)

                inferred_without_cast = self.typeinfer_term(casted_term)
                inferred_without_cast_long = self.replace_sort_def(inferred_without_cast)

                if sub(inferred_without_cast_long, casted_to_long):
                    raise L4TypeInferError(t,
                    f"Unnecessary typecast of {sort_expanded_display(inferred_without_cast,inferred_without_cast_long)} " 
                                            f"to {sort_expanded_display(casted_to,casted_to_long)}" )

                if not self.cast_allowed(casted_term, inferred_without_cast_long, casted_to_long):
                    raise L4TypeInferError(t,
                    # print(
                    f"Term {casted_term} of sort {sort_expanded_display(inferred_without_cast, inferred_without_cast_long)} cannot be cast to "
                    f"{sort_expanded_display(casted_to, casted_to_long)}; see TypeChecker.cast_allowed.")

                if t.fnsymb_name == "units":
                    assert isinstance(casted_term,Literal)
                    if self.verbose:
                        print(f"Innocent cast {sort_expanded_display(inferred_without_cast,inferred_without_cast_long)} to "
                              f"{sort_expanded_display(casted_to, casted_to_long)} for literal {casted_term}.")
                elif t.fnsymb_name in {"check","cast"}:
                    if self.verbose:
                        print(f"Z3-to-check cast {sort_expanded_display(inferred_without_cast,inferred_without_cast_long)} to "
                              f"{sort_expanded_display(casted_to, casted_to_long)} for non-literal.")
                else:
                    print(
                        f"UNCHECKED cast {sort_expanded_display(inferred_without_cast,inferred_without_cast_long)} to "
                        f"{sort_expanded_display(casted_to, casted_to_long)} for non-literal. See {t.coord} of {self.prog.filename}")
                return casted_to_long

            argsorts = tuple(self.typeinfer_term_then_sortdef_subst(arg) for arg in t.args)

            # ------------Tuples------------
            if t.fnsymb_name == "tuple":
                assert len(argsorts) > 1
                return SortOpApp('Tuple', argsorts)
            elif t.fnsymb_name == "tupleGet":
                tuplesort = cast(SortOpApp, argsorts[0])
                indlit = t.args[1]
                if isinstance(indlit, IntLit) and indlit.lit <= len(tuplesort.args):
                    return cast(Sort,tuplesort.args[indlit.lit]) # cast shouldn't be necessary...
                else:
                    raise L4TypeInferError(t, "Problem with tupleGet.")

            # ------------TDMap------------
            # if t.fnsymb_name in {'mapSet','tdGEQ','mapDelete','mapHas','nonempty','empty','emptyTDMap'}:
            #     if t.fnsymb_name == 'mapSet':

            # ------------Other function applications------------
            rv: Optional[Sort]
            fnsymb_type = STANDARD_FNTYPES[t.fnsymb_name]
            if fnsymb_type:
                try:
                    if t.fnsymb_name in ASSOCIATIVE_OPS and len(argsorts) > 2:
                        rv = self.overloaded_fnapp_range_memo(fnsymb_type, argsorts[0:2], t)
                        for i in range(2,len(argsorts)-1):
                            assert rv is not None
                            rv = self.overloaded_fnapp_range_memo(fnsymb_type, (rv, argsorts[i]), t)
                    elif t.fnsymb_name in CHAIN_PREDS and len(argsorts) > 2:
                        for i in range(0,len(argsorts)-2):
                            assert self.overloaded_fnapp_range_memo(fnsymb_type, argsorts[i:i+2], t) == 'Bool'
                        rv = 'Bool'
                    else:
                        rv = self.overloaded_fnapp_range_memo(fnsymb_type, argsorts, t)
                except L4TypeError as e:
                    if isinstance(e,(L4TypeError,TransitivelyClosedDirectedGraphInvariantError)):
                        raise e
                    else:
                        raise L4TypeInferError(t, str(e.args[0]))
            else:
                raise L4TypeInferError(t, f"Function symbol {t.fnsymb_name} has no type.")
            if rv is None:
                raise L4TypeInferError(t,
                                       f"overloaded_fnapp_range_memo return None.\nArg sorts: {argsorts}\nFn type:\n{fnsymb_type}")
            return rv

        elif isinstance(t,Literal):
            # ------------Literals------------
            if isinstance(t,IntLit):
                if t.lit == 0:
                    return "{0}"
                elif t.lit == 1:
                    return "{1}"
                elif t.lit > 0:
                    return "PosInt"
                return "Int"
            elif isinstance(t,FloatLit):
                if t.lit == 0:
                    return "{0}"
                elif t.lit == 1:
                    return "{1}"
                elif 0 < t.lit < 1:
                    return "Fraction(0,1)"
                elif t.lit > 1:
                    return "PosReal"
                return "Real"
            elif isinstance(t,BoolLit):
                return "Bool"
            elif isinstance(t,SimpleTimeDeltaLit):
                if t.num > 0:
                    return "PosTimeDelta"
                else:
                    return "TimeDelta"
            elif isinstance(t, TimeDeltaLit):
                if t.lit.total_seconds() > 0:
                    return "PosTimeDelta"
                else:
                    return "TimeDelta"
            elif isinstance(t,DateTimeLit):
                return "DateTime"
            elif isinstance(t,RoleIdLit):
                return "RoleId"
            elif isinstance(t,StringLit):
                return "String"

        # ------------Variables------------
        elif isinstance(t, (LocalVar, StateVar, PrimedStateVar)):
            return t.vardec.sort
        elif isinstance(t, ActionBoundActionParam):
            return t.action.param_sorts_by_name[t.name]
        elif isinstance(t,ContractParam):
            return t.paramdec.sort
        elif isinstance(t,RuleBoundActionParam):
            action = self.prog.action(t.action_rule.action_id)
            return action.param_sort(t.ind)


        raise Exception(f"typeinfer_term unhandled case for {type(t)}: {t}" )

    def typecheck_term(self, t:Term, s:Sort) -> bool:
        s = self.replace_sort_def(s)
        inferred = self.typeinfer_term_then_sortdef_subst(t)

        # we want to infer types of ambiguous numeric literals, at least sometimes
        # if s is a simple units type compatible with t, we'll allow it.
        if ((isinstance(t,IntLit) or isinstance(t,FloatLit)) and
             isinstance(s,SortOpApp) and
             s.op == "Dimensioned" and
             s.args[0] in NormalUnboundedNumericSorts and
             sub(inferred, s.args[0]) ):
            return True

        assert inferred is not None
        if not sub(inferred,s):
            print(f"{inferred} not ≤ {s}")
            print(t)
            raise L4TypeInferCheckError(t,inferred,s)
        return True

    def typecheck_statement(self, s:Statement):
        if isinstance(s, LocalVarDec):
            self.typecheck_term(s.value_expr,s.sort)
        elif isinstance(s, StateVarAssign):
            self.typecheck_term(s.value_expr,s.vardec.sort)
        elif isinstance(s, IfElse):
            self.typecheck_term(s.test,'Bool')
            for statement in s.true_branch:
                self.typecheck_statement(statement)
            if s.false_branch:
                for statement in s.false_branch:
                    self.typecheck_statement(statement)
        elif isinstance(s, FVRequirement):
            self.typecheck_term(s.value_expr,'Bool')
        else:
            raise NotImplementedError

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


    def typecheck_situation(self, situation:Situation, verbose=True):
        if verbose:
            msg2 = f"Typechecking Situation {situation.situation_id}"
            print(msg2 + "\n" + "-" * len(msg2))
        for rule in situation.action_rules():
            self.typecheck_action_rule(rule)

    def typecheck_action(self, action:Action, verbose=True):
        if verbose:
            msg2 = f"Typechecking Action {action.action_id}"
            print(msg2 + "\n" + "-" * len(msg2))
        for prop in action.preconditions:
            self.typecheck_term(prop, 'Bool')
        for statement in action.state_transform_statements():
            self.typecheck_statement(statement)
        for rule in action.future_action_rules():
            assert isinstance(rule,ActionRule)
            self.typecheck_action_rule(rule)

