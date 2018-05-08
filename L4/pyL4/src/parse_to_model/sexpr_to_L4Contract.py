import logging

import dateutil.parser
from mypy_extensions import NoReturn
from copy import deepcopy

from src.hard_correctness_checks.normal_forms import eliminate_must
from src.model.EventsAndTraces import breachActionId, interveneOnDelayId
from src.independent.util_for_sequences import flatten
from src.independent.util_for_str import nonemptySortedSubsets
from src.parse_to_model.floating_rules_transpile import floating_rules_transpile_away
from src.independent.util import chcaststr, todo_once, castid, chcast
from src.independent.util_for_str import streqci, isFloat, isInt
from src.constants_and_defined_types import *
from src.correctness_checks import L4ContractConstructorInterface
from src.independent.FileCoord import FileCoord
from src.independent.SExpr import SExpr, SExprOrStr, sexpr_rewrite
from src.independent.parse_sexpr import castse, STRING_LITERAL_MARKER, prettySExprStr
from src.independent.typing_imports import *
from src.model.Action import Action
from src.model.ActionRule import FutureActionRuleType, PartyFutureActionRule, ActionRule, NextActionRule, \
    EnvNextActionRule, \
    PartyNextActionRule
from src.model.BoundVar import ContractParam, RuleBoundActionParam, ActionBoundActionParam, \
    LocalVar, StateVar, PrimedStateVar
from src.model.ContractClaim import ContractClaim, StateInvariant
from src.model.ContractParamDec import ContractParamDec
from src.model.Definition import Definition
from src.model.StateTransform import StateTransform
from src.model.Statement import Statement, FVRequirement, \
    LocalVarDec, IfElse, StateVarAssign, Block
from src.model.StateVarDec import StateVarDec
from src.model.L4Contract import L4Contract, is_derived_destination_id, is_derived_trigger_id, \
    derived_trigger_id_to_situation_id, derived_destination_id
from src.model.L4Macro import L4Macro, L4BlockMacro
from src.model.Literal import SortLit, IntLit, FloatLit, BoolLit, SimpleTimeDeltaLit, DateTimeLit, \
    RoleIdLit, Literal, TimeDeltaLit, StringLit
from src.model.Situation import Situation
from src.model.Sort import Sort, SortOpApp
from src.model.Term import FnApp
from src.model.Term import Term

def primed(s:str) -> StateVarId:
    return s + "'" # type:ignore
def unprimed(s:str) -> StateVarId:
    assert s[-1] == "'"
    return s[:-1] # type:ignore
def isprimed(s:str) -> bool:
    return s[-1] == "'"

def syntaxErrorX(expr: Optional[SExprOrStr], msg:Optional[str] = None) -> NoReturn:
    if isinstance(expr,SExpr):
        raise SyntaxError((msg if msg else "") +
                          "\n" + str(expr) +
                          "\nline " + str(expr.line))
    elif expr is not None:
        raise SyntaxError((msg if msg else "") +
                          "\n" + expr )
    else:
        raise SyntaxError((msg if msg else ""))

T = TypeVar('T')

IntBounds = Tuple[int,Optional[int]]

class L4ContractConstructor(L4ContractConstructorInterface):
    def __init__(self, filename:str, verbose=True,
                 flags:Optional[Dict[str,bool]] = None,
                 raw_substitutions: Optional[Dict[str, Any]] = None) -> None:
        self.top : L4Contract = L4Contract(filename)
        self.referenced_nonderived_situation_ids: Set[SituationId] = set()
        self.referenced_nonderived_action_ids: Set[ActionId] = set()
        self.after_model_build_requirements : List[Tuple[Callable[[],bool],str]] = []
        self.verbose = verbose

        self._building_situation_id : Optional[SituationId] = None
        self._building_action_id: Optional[ActionId] = None
        self._building_next_action_rule: bool = False
        self._building_future_action_rule: bool = False

        self.flags = flags
        self.raw_substitutions = raw_substitutions

    def addAfterBuildAssertion(self, f:Callable[[],bool], errmsg:str):
        self.after_model_build_requirements.append((f,errmsg))

    def _handle_flags(self, l:List[SExpr]) -> List[SExpr]:
        assert self._needs_preprocessing()
        flags = cast(Dict[str, bool], self.flags)
        rs = cast(Dict[str, Any], self.raw_substitutions)

        def helper(x:SExprOrStr) -> List[SExprOrStr]:
            if isinstance(x,str):
                if rs is not None and x in rs:
                    return [rs[x]]
                else:
                    return [x]
            if len(x) == 0:
                return [x]
            results : List[SExprOrStr] = []
            children_to_recurse_on : List[SExprOrStr]
            if x[0] == "ifflag":
                assert len(x) == 3 or len(x) == 5, f"ifflag expression has wrong form: {x}\nexpected\n" \
                                                   "( ifflag ‹flag› (‹expr1> <expr2> ...) else (<expr1> <expr2> ...) )\n" \
                                                   "or\n" \
                                                   "( ifflag ‹flag› (‹expr1> <expr2> ...) )\n"

                assert x[1] in flags
                if flags[x[1]]:
                    children_to_recurse_on = x[2]
                elif len(x) == 5:
                    # if test false and has else part
                    assert x[3] == "else", f"expected ({x[0]} ‹flag› ‹block› else ‹block›)"
                    children_to_recurse_on = x[4]
                else:
                    # if test false and no else part
                    children_to_recurse_on = []
                for c in children_to_recurse_on:
                    results.extend(helper(c))
                return results
            else:
                children_to_recurse_on = x.lst
                for c in children_to_recurse_on:
                    results.extend(helper(c))
                return [SExpr(results, x.line, x.col, x.symb)]

        rv = []
        for x in l:
            rv.extend(helper(x))
        return cast(List[SExpr],rv)


    def syntaxError(self, expr: SExprOrStr, msg:Optional[str] = None) -> NoReturn:
        if isinstance(expr,SExpr):
            raise SyntaxError((msg if msg else "") +
                              "\n" + str(expr) +
                              "\nline " + str(expr.line) +
                              "\n" + str(self.top.filename))
        else:
            print(expr, type(expr))
            print(self.top.filename, type(self.top.filename))
            raise SyntaxError((msg if msg else "") +
                              "\n" + str(expr) +
                              "\n" + str(self.top.filename))

    def assertOrSyntaxError(self, test:bool, expr:SExpr, msg:Optional[str] = None) -> Union[NoReturn,None]:
        if not test:
            self.syntaxError(expr, msg)
        return None

    @staticmethod
    def assertOrSyntaxErrorX(test:bool, expr:Optional[SExpr], msg:Optional[str] = None) -> Union[NoReturn,None]:
        if not test:
            syntaxErrorX(expr, msg)
        return None

    def _mk_toplevel(self, x:SExpr):
        rem = x.tillEnd(1)

        # def head(constant: str) -> bool:
        #     nonlocal x
        #     return streqci(x[0], constant)
        def head(*constants: str) -> bool:
            nonlocal x
            return not all( not streqci(x[0], constant) for constant in constants )

        if head(MACRO_DEC_LABEL) or head(BLOCKMACRO_DEC_LABEL):
            assert isinstance(x[1],str) and len(x) >= 4, "Macro should have the form "\
                "(Macro ‹macroname› (‹param1› ‹param2› ...) ‹sexpr1›) or " \
                "(BlockMacro ‹macroname› (‹param1› ‹param2› ...) ‹sexpr1› <sexpr2> ...)"
            macroname = chcaststr(x[1])
            macroparams: List[str]
            if isinstance(x[2], str):
                macroparams = [x[2]]
            else:
                macroparams = cast(List[str], castse(x[2]).lst)

            if head(MACRO_DEC_LABEL):
                macrobody = chcast(SExpr, x[3])
                self.top.macros[macroname] = L4Macro(macroparams, macrobody)
            else:
                self.top.blockmacros[macroname] = L4BlockMacro(macroparams, chcast(list, x[3:]))


        elif head(GLOBAL_VARS_AREA_LABEL) or head("GlobalStateVars"):
            self.top.state_var_decs = self._mk_statevar_decs(rem)

        elif head(CONTRACT_PARAMETERS_AREA_LABEL):
            self.top.contract_params = {castid(ContractParamId, expr[0]): self._mk_contract_param(expr) for expr in rem}

        elif head(ROLES_DEC_LABEL) or head("Actors"):
            self.top.roles.extend(self._mk_actors(rem))

        elif head(PROSE_CONTRACT_AREA_LABEL):
            self.top.prose_contract = self._mk_prose_contract(cast(List[List[str]], rem))

        elif head(FORMAL_CONTRACT_AREA_LABEL, "Actions&Situations", "Situations&Actions"):
            self._mk_main_program_area(rem)

        elif head(TIMEUNIT_DEC_LABEL):
            given = chcaststr(x[1]).lower()
            self.assertOrSyntaxError(
                given in SUPPORTED_TIMEUNITS or given in LONGFORMS_OF_SUPPORTED_TIMEUNITS, x,
                f"{TIMEUNIT_DEC_LABEL} must be one of {SUPPORTED_TIMEUNITS} or {list(LONGFORMS_OF_SUPPORTED_TIMEUNITS.values())}")
            if given in SUPPORTED_TIMEUNITS:
                self.top.timeunit = given
            else:
                self.top.timeunit = LONGFORMS_OF_SUPPORTED_TIMEUNITS[given]

        elif head("StartDatetime","StartDT"):
            # print("start datetime?", rem)
            try:
                dt = dateutil.parser.parse(rem[0][1])
                print("the dt", dt)
                # return DateTimeLit(dt, rem.coord())
                self.top.start_datetime = dt
            except Exception as e:
                print(rem,e)
                raise e

        elif head(SORT_DEFINITIONS_AREA) or head("TypeDefinitions"):
            self._mk_sort_definitions(rem)

        elif head(DEFINITIONS_AREA):
            self.top.definitions = self._mk_definitions(rem)

        elif head(TOPLEVEL_CLAIMS_AREA_LABEL):
            self.top.claims = self._mk_claims(rem)

        elif head(TOPLEVEL_STATE_INVARIANTS_AREA_LABEL):
            self.top.state_invariants = self._mk_invariants(rem)

        elif head("VerificationDefinition"):
            return

        elif head(DOT_FILE_NAME_LABEL):
            self.top.dot_file_name = chcaststr(
                x[1][1])  # the extra [1] is because its parse is of the form ['STRLIT', 'filename']
        elif head(IMG_FILE_NAME_LABEL):
            self.top.img_file_name = chcaststr(
                x[1][1])  # the extra [1] is because its parse is of the form ['STRLIT', 'filename']

        elif head("TypedMacro"):
            todo_once("Handle TypedMacro")

        elif head("NLGNames"):
            for pair in x[1:]:
                self.top.nlg_names[pair[0]] = pair[1][1]

        # elif head("Flags"):
            # self._flags = set(x[1:])
            # return

        else:
            raise Exception("Unsupported: ", x[0])

    def _needs_preprocessing(self) -> bool:
        return (self.flags is not None and len(self.flags) > 0) or (self.raw_substitutions is not None and len(self.raw_substitutions) > 0)

    def mk_l4contract(self, l:List[SExpr]) -> L4Contract:
        if self._needs_preprocessing():
            l = self._handle_flags(l)
            # for x in l:
            #     print(prettySExprStr(x))

        for sexpr in  l:
            eliminate_must(sexpr, self.top.timeunit, "1d")

        for x in l:
            self._mk_toplevel(x)

        for f in self.after_model_build_requirements:
            if not f[0]():
                raise Exception(f[1])

        roles_without_Env = self.top.roles.copy()
        roles_without_Env.remove(ENV_ROLE)
        roles_without_Env.remove(ARBITER_ROLE)
        for role_lst in nonemptySortedSubsets(roles_without_Env):
            sit = Situation.breachSituation(*role_lst)
            self.top.situations_by_id[sit.situation_id] = sit
            act = Action.breachAction(*role_lst)
            self.top.actions_by_id[act.action_id] = act
        for roleid in roles_without_Env:
            if roleid != ARBITER_ROLE:
                act = Action.interveneOnDelayAction(roleid)
                self.top.actions_by_id[act.action_id] = act

        # don't impose this for rule vars.
        # for rule in self.top.action_rules():
        #     if rule.arg_vars_bound_by_rule:
        #         for rule_varname in rule.arg_vars_bound_by_rule:
        #             sort = rule.rule_varname_to_sort(self.top,rule_varname)
        #             self.top.register_sorted_name(rule_varname, sort)

        floating_rules_transpile_away(self.top, self.verbose)
        return self.top

    def _mk_sort(self, x:SExprOrStr) -> Sort:
        sort: Sort
        if isinstance(x,str):
            # sort = temp_normalize_sort(castid(SortId, x))
            sort = castid(SortId, x)
        else:
            if len(x) == 2 and x[0] == STRING_LITERAL_MARKER:
                # This is just a sort that has brackets/parentheses/spaces in it, so that it has to be
                # enclosed in quotes in source code.
                sort = self._mk_sort(x[1])
            else:
                assert len(x) >= 2
                sort = SortOpApp.c(x[0], tuple(self._mk_sort(x[i]) for i in range(1, len(x))))
        self.top.sorts.add(sort)
        assert sort is not None
        return sort

    def mk_sort_lit(self, x:SExprOrStr) -> SortLit:
        if isinstance(x,str):
            # return SortLit(temp_normalize_sort(x))
            return SortLit(x)
        else:
            assert len(x) == 2 and x[0] == STRING_LITERAL_MARKER
            # assert x[1] in AllSorts
            return SortLit(x[1])

    def _mk_contract_param(self, expr:SExpr) -> ContractParamDec:
        # self.assertOrSyntaxError( len(expr) == 5, expr, "Contract parameter dec should have form (name : sort := term)" )
        sort = self._mk_sort(expr[2])
        if len(expr) == 5:
            return ContractParamDec(expr[0], sort, self._mk_term(expr[4], None, None, None, expr))
        else:
            return ContractParamDec(expr[0], sort, None)

    # def _handle_ifflag(self, x:SExprOrStr) -> Optional[SExprOrStr]:
    #     assert len(x) == 3 or len(x) == 5
    #     assert x[1] in self.flags
    #     if x[1]:
    #         return x[2]
    #     elif len(x) == 5:
    #         assert x[3] == "else", "expected ifflag ‹flag› ‹body1› else ‹body2›"
    #         return x[4]
    #     else:
    #         return None

    def _mk_statevar_decs(self, l:SExpr) -> Dict[StateVarId, StateVarDec]:
        rv : Dict[StateVarId, StateVarDec] = dict()
        for dec in l:
            try:
                i = 0
                modifiers : List[str] = []
                while True:
                    assert isinstance(dec[i],str), dec
                    if dec[i].lower() in VARIABLE_MODIFIERS:
                        modifiers.append(chcaststr(dec[i].lower()))
                        i += 1
                    else:
                        break
                name = cast(StateVarId, chcaststr(dec[i]))
                sort = self._mk_sort(dec[i + 2])

                initval : Optional[Term] = None
                if i+3 < len(dec) and (dec[i+3] == ':=' or dec[i+3] == '='):
                    initval = self._mk_term(dec[i + 4],None,None,None,dec)

                rv[name] = StateVarDec(name, sort, initval, modifiers)
                # I had written: TO DO: for requiring primed variables.
                # rv[primed(name)] = rv[name]

                self.top.write_bounds[name] = self._modifiers_to_write_bounds(modifiers, l)
                self.top.register_sorted_name(name, sort)

            except Exception as e:
                logging.error("Problem processing " + str(dec))
                raise e

        # logging.info(str(rv))
        return rv

    def _modifiers_to_write_bounds(self, modifiers:Iterable[str], parent_expr:SExpr) -> IntBounds:
        low,high = 0, None
        low2:int
        high2:Optional[int]
        for s in modifiers:
            if s == "writeonce" or s == "writes1":
                low2,high2 = 1,1
            elif s == "writeatmostonce" or s == "writes≤1":
                low2,high2 = low,1
            else:
                low2,high2 = low,high

            self.assertOrSyntaxError( low2 >= low and (high is None or high2 <= high), parent_expr )
            low,high = low2,high2
        return low,high

    def _mk_claims(self, l:SExpr) -> List[ContractClaim]:
        rv = [ContractClaim(x) for x in l]
        return rv

    def _mk_invariants(self, l:SExpr) -> List[StateInvariant]:
        # rv = [ContractClaim(self.term(x)) for x in l]
        rv = [StateInvariant(self._mk_term(x,None,None,None,x)) for x in l]
        # logging.info(str(rv))
        return rv

    def _mk_actors(self, s:SExpr) -> List[RoleId]:
        # logging.info(str(l))
        self.assertOrSyntaxError(all(isinstance(x, str) for x in s), s, "Actors declaration S-expression should have the form (Actors Alice Bob)")
        return cast(List[RoleId],s.lst.copy())

    def _mk_definitions(self, s:SExpr) -> Dict[DefinitionId, Definition]:
        self.assertOrSyntaxError(all(len(x) == 3 for x in s), s,
                                 "Definition declaration S-expressions should have the form (id = SExpr)")
        return {x[0] : Definition(x[0],x[2]) for x in s}

    def _mk_sort_definitions(self, s:SExpr) -> None:
        self.assertOrSyntaxError(all(len(x) == 3 for x in s), s,
                                 "Definition declaration S-expressions should have the form (id = SExpr) or (id := SExpr)")
        for x in s:
            self.top.sort_definitions[x[0]] = self._mk_sort(x[2])

        self._expand_sort_defns()

    def _expand_sort_defns(self):
        expanded: Dict[str,Optional[Sort]] = {sid: None for sid in self.top.sort_definitions}
        def helper(sort:Sort) -> Sort:
            if isinstance(sort,str):
                if sort in expanded:
                    expansion = expanded[sort]
                    assert expansion is not None, "Do you have a cycle in your sort definitions? Or do you need to reorder them?"
                    # assert expansion is not None, f"{sort}\n{self.top.sort_definitions}\n{expanded}"
                    return expansion
                else:
                    return sort
            else:
                assert isinstance(sort, SortOpApp)
                if sort.op == "Dimensioned":
                    # need to exclude the second arg to Dimensioned, since it's the string name of this defined sort.
                    return SortOpApp.c(sort.op, (helper(sort.args[0]), sort.args[1]))
                else:
                    return SortOpApp.c(sort.op, tuple(helper(x) for x in sort.args))

        for defined_sort_id, sort_defn in self.top.sort_definitions.items():
            expanded[defined_sort_id] = helper(sort_defn)

        assert all(expanded[x] is not None for x in expanded)
        self.top.expanded_sort_definitions = cast(Dict[str,Sort], expanded)

    def _mk_prose_contract(self, l: List[List[str]]) -> ProseContract:
        rv = {castid(ProseClauseId,x[0]): x[1] for x in l}
        # logging.info(str(rv))
        return rv

    def handle_apply_macro(self, x:SExpr) -> SExpr:
        macroname, args = (chcaststr(x[1]),x[2]) if x[0] == APPLY_MACRO_LABEL else (x[0],x[1])
        macro : L4Macro = self.top.macros[macroname]
        if isinstance(args,str):
            return macro.subst([args])
        else:
            return macro.subst(args)

    def handle_apply_blockmacro(self, x:SExpr) -> List[SExpr]:
        macroname, args = (chcaststr(x[1]),x[2]) if x[0] == APPLY_MACRO_LABEL else (x[0],x[1])
        macro : L4BlockMacro = self.top.blockmacros[macroname]
        assert isinstance(macro, L4BlockMacro)
        if isinstance(args,str):
            return macro.subst([args])
        else:
            return macro.subst(args)

    def _is_anymacro_app(self, s:SExprOrStr) -> bool:
        return isinstance(s, SExpr) and isinstance(s[0], str) and \
               (s[0] in self.top.macros or s[0] in self.top.blockmacros) or\
               (s[0] == APPLY_MACRO_LABEL and (s[1] in self.top.macros or s[1] in self.top.blockmacros))

    def _is_macro_app(self, s:SExprOrStr):
        return isinstance(s, SExpr) and isinstance(s[0], str) and \
               s[0] in self.top.macros or \
               (s[0] == APPLY_MACRO_LABEL and s[1] in self.top.macros)

    def _is_blockmacro_app(self, s:SExprOrStr):
        return isinstance(s, SExpr) and isinstance(s[0], str) and \
               s[0] in self.top.blockmacros or \
               (s[0] == APPLY_MACRO_LABEL and s[1] in self.top.blockmacros)

    def _with_macro_handling(self, e:Union[SExprOrStr, Sequence[SExprOrStr]],
                             g:Callable[[SExprOrStr,Any],T], args:Any) -> Union[T,List[T]]:
        if isinstance(e,list):
            return flatten([self._with_macro_handling(x, g, args) for x in e])
        elif isinstance(e,SExpr):
            if self._is_macro_app(e):
                return self._with_macro_handling(self.handle_apply_macro(e), g, args)
            elif self._is_blockmacro_app(e):
                return self._with_macro_handling(self.handle_apply_blockmacro(e), g, args)
            return [g(e, *args)]
        elif isinstance(e,str):
            return [g(e, *args)]
        else:
            raise Exception



    def _mk_main_program_area(self, l:SExpr) -> None:
        self.assertOrSyntaxError(l[0][0] == STRING_LITERAL_MARKER, l[0], f"Immediately after the {FORMAL_CONTRACT_AREA_LABEL} keyword should be a string literal that gives the contract's name")
        self.top.contract_name = chcaststr(l[0][1]) # [1] because STRLIT sexpr
        x: SExpr

        for x in l[1:]:
            assert len(x) >= 2
            if self._is_macro_app(x):
                x = self.handle_apply_macro(x)
            self._mk_main_program_area_part(x,l)

    def _mk_main_program_area_part(self, x:SExpr, l:SExpr):
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if self._is_macro_app(x):
                x = self.handle_apply_macro(x)

            if head(START_SITUATION_LABEL):
                self.assertOrSyntaxError( len(x) == 2, l, "StartState declaration S-expression should have length 2")
                situation_id = cast(SituationId, chcaststr(x[1]))
                self.top.start_situation_id = situation_id
                if not is_derived_destination_id(situation_id):
                    self.referenced_nonderived_situation_ids.add(situation_id)

            elif head(ACTION_LABEL):
                action_id : ActionId
                action : Action
                action_body = x.tillEnd(2)
                if isinstance(x[1], str):
                    # e.g. (Action SomethingHappens ...)
                    action_id = cast(ActionId, x[1])
                    action = self._mk_action(action_id, None, action_body)
                    self.top.actions_by_id[action_id] = action
                else:
                    # e.g. (Action (SomethingHappens param&sort1 param&sort2) ...)
                    action_id = cast(ActionId, chcaststr(x[1][0]))
                    action_params = cast(List[List[str]], x[1][1:])
                    action = self._mk_action(action_id, action_params, action_body)
                    self.top.actions_by_id[action_id] = action
                self.top.ordered_declarations.append(action)

            elif head(SITUATION_LABEL) or head("StateType") or head("SituationType"):
                situation_id = cast(SituationId, chcaststr(x[1]))
                situation_data = x.tillEnd(2)
                situation = self._mk_situation(situation_id, situation_data, None)
                self.top.situations_by_id[situation_id] = situation
                self.top.ordered_declarations.append(situation)

            else:
                self.syntaxError(l, f"Unrecognized head {x[0]}")


    def _mk_situation(self, situation_id:SituationId, rest:SExpr, parent_action:Optional[Action]) -> Situation:
        situation = Situation(situation_id)
        self._building_situation_id = situation_id
        x: SExpr
        for x in rest:
            assert isinstance(x,SExpr), f"{x} should be an s-expression"

            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if head(SITUATION_PRECONDITION_LABEL):
                situation.preconditions.append(self._mk_term(x[1], situation, parent_action,None, x))

            elif head(SITUATION_DESCRIPTION_LABEL):
                situation.description = chcaststr(x[1][1]) # extract from STRLIT expression

            elif head(PROSE_REFS_LABEL):
                situation.prose_refs = cast(List,x[1:]).copy()

            elif 'visits' in x or 'traversals' in x:
                situation.visit_bounds = x # self.term(x, None, situation)

            elif head("possibly-from-earlier"):
                assert x[1] in self.top.roles
                assert x[2] in ["must-later", "may-later"]
                floating_rule_type = FutureActionRuleType(x[1], x[3], x[2])
                situation.possible_floating_rule_types.add(floating_rule_type)
                self.top.possible_floating_rule_types.add(floating_rule_type)

            elif head("nlg"):
                situation.nlg = x[1][1]
            elif head("nlgsection"):
                situation.nlgsection = x[1][1]

            elif head(OUT_CONNECTIONS_LABEL):
                if isinstance(x[1],SExpr) and isinstance(x[1][0],str) and (x[1][0] == 'guardsDisjointExhaustive' or x[1][0] == 'timeConstraintsPartitionFuture'):
                    x = x[1]
                    todo_once("guardsDisjointExhaustive etc in situation()")

                action_rule_exprs = x.tillEnd(1).lst
                for action_rule_expr in action_rule_exprs:
                    self._with_macro_handling(action_rule_expr, self._mk_next_action_rule, (situation, parent_action)) # type:ignore

            else:
                if head('guardsDisjointExhaustive') or head('timeConstraintsPartitionFuture'):
                    x = x.tillEnd(1)
                    todo_once("guardsDisjointExhaustive etc in situation()")

                self._with_macro_handling(x, self._mk_next_action_rule, (situation, parent_action)) # type:ignore
                # self.syntaxError(x, f"Unsupported declaration type {x[0]} in situation {situation_id}")
                # todo_once(f"Handle {x[0]} in situation dec")

        self._building_situation_id = None
        return situation

    def _mk_action(self, action_id:ActionId, params_sexpr:Optional[List[List[str]]], rest:SExpr) -> Action:
        a = Action(action_id)
        self._building_action_id = action_id
        dest_situation_id = None
        x: SExpr
        if params_sexpr is not None: #isinstance(params_sexpr, SExpr):
            a.param_sorts_by_name = self._mk_action_params(params_sexpr)
            for name,sort in a.param_sorts_by_name.items():
                self.top.register_sorted_name(name,sort)
            a.param_names = [castid(ActionParamId, y[0]) for y in params_sexpr]
            a.param_name_to_ind = {a.param_names[i]: i for i in range(len(a.param_names))}
        for x in rest:
            def head(constant:str) -> bool:
                nonlocal x
                if len(x) == 0:
                    print("problem", x)
                return streqci(x[0], constant)

            if head(ACTION_PRECONDITION_LABEL):
                a.preconditions.append(self._mk_term(x[1], None, a, None, rest))

            elif head(ACTION_POSTCONDITION_LABEL):
                a.postconditions.append(self._mk_term(x[1], None, a, None, rest))

            elif head(ACTION_DESCRIPTION_LABEL):
                a.action_description = chcaststr(x[1][1]) # extract from STRLIT expression

            elif head(CODE_BLOCK_LABEL):
                a.state_transform= self._mk_state_transform(cast(List[SExpr], x.tillEnd(1).lst), a)

            elif head(PROSE_REFS_LABEL):
                a.prose_refs  = cast(List[str], x.tillEnd(1).lst)

            elif head(TRANSITIONS_TO_LABEL):
                dest_situation_id = x[1]
                if not is_derived_destination_id(dest_situation_id) and dest_situation_id != LOOP_KEYWORD:
                    self.referenced_nonderived_situation_ids.add(dest_situation_id)

            elif 'traversals' in x or 'visits' in x:
                a.traversal_bounds = x # self.term(x, None, a)

            elif head("nlg"):
                a.nlg = x[1][1]
            elif head("nlgsection"):
                a.nlgsection = x[1][1]

            elif head('Future'):
                a.futures = self._mk_futures(x.tillEnd(1), a)

            elif head(ALLOWED_SUBJECTS_DEC_LABEL):
                a.allowed_subjects = x.tillEnd(1)

            elif head(FOLLOWING_SITUATION_DEC_LABEL):
                anon_sect_id : str
                if is_derived_trigger_id(action_id):
                    anon_sect_id = derived_trigger_id_to_situation_id(action_id)
                else:
                    anon_sect_id = derived_destination_id(action_id)
                a.following_anon_situation  = self._mk_situation(anon_sect_id, x.tillEnd(1), a)
                a.following_anon_situation.parent_action_id = action_id
                self.top.situations_by_id[a.following_anon_situation.situation_id] = a.following_anon_situation

            elif head("AllowedRoles"):
                todo_once("handle (AllowedRoles ...)")
            elif head("nlg"):
                todo_once("handle (nlg ...)")
            else:
                self.syntaxError(f"Unhandled {x[0]}",str(x))
                todo_once(f"Handle {x[0]} in action dec.")

        if dest_situation_id:
            a.dest_situation_id = dest_situation_id
        else:
            if is_derived_trigger_id(a.action_id):
                a.dest_situation_id = derived_trigger_id_to_situation_id(a.action_id)
                self.referenced_nonderived_situation_ids.add(a.dest_situation_id)
            else:
                a.dest_situation_id = derived_destination_id(a.action_id)

        self._building_action_id = action_id
        return a

    def _mk_futures(self, rules:SExpr, src_action:Action) -> List[PartyFutureActionRule]:
        rv : List[PartyFutureActionRule] = []
        for action_rule_expr in rules:
            rv.extend(self._with_macro_handling(action_rule_expr, self._mk_future_action_rule, (src_action,))) # type:ignore
        return rv

    def _mk_action_params(self, parts:List[List[str]]) -> ParamsDec:
        pdec : List[str]
        rv : ParamsDec = dict()
        for pdec in parts:
            assert len(pdec) == 3, f"Expected [<param name str>, ':', SORTstr] but got {pdec}"
            sort = self._mk_sort(pdec[2])
            rv[castid(ActionParamId,pdec[0])] = sort
            self.top.register_sorted_name(pdec[0],sort)

        return rv

    def _mk_state_transform(self, statement_exprs:List[SExpr], a:Action) -> StateTransform:
        return StateTransform(self._mk_statements(statement_exprs, a, None))

    def _mk_statements(self, statement_exprs:List[SExpr], parent_action:Action, parent_ifelse:Optional[IfElse]) -> List[Statement]:
        """
        call _mk_statement on each element x of statement_exprs and return list of results, UNLESS x is a macro app, in which case:
        if x is a seqmacro,
            seqexpansion = expansion of x
            extend return list with result of calling _mk_statements on seqexpansion
        if x is a regular macro
            seqexpansion = [expansion of x]
            extend return list with result of calling _mk_statements on seqexpansion
        """
        rv = []
        for statement_expr in statement_exprs:
            assert not isinstance(statement_expr,list)
            if self._is_blockmacro_app(statement_expr):
                rv.extend(self._mk_statements(self.handle_apply_blockmacro(statement_expr), parent_action, parent_ifelse))
            elif self._is_macro_app(statement_expr):
                rv.extend(self._mk_statements([self.handle_apply_macro(statement_expr)], parent_action, parent_ifelse))
            else:
                it = self._mk_statement(statement_expr, parent_action, rv, parent_ifelse)
                rv.append(it)

            # if isinstance(rv[-1],IfElse):
            #     print(rv[-1].next_statement())
            #     # print(rv[-1])
            #     assert statement_expr == statement_exprs[-1], "For now (not too much work to lift this), " \
            #                                                   "(if Term Block else Block) can only appear as the last " \
            #                                                   "statement in a Block." \
            #                                                   "I don't remember if there's a reason for that besides for symbolic execution."
        return rv

    def _mk_statement(self, statement_expr:SExpr, parent_action:Action, parent_block:Block, parent_ifelse:Optional[IfElse]) -> Statement:
        assert isinstance(statement_expr, SExpr) and statement_expr.coord is not None, statement_expr
        varname : str
        rv : Statement
        try:
            assert not self._is_anymacro_app(statement_expr)

            if statement_expr[0] == 'conjecture' or statement_expr[0] == 'prove':
                self.assertOrSyntaxError(len(statement_expr) == 2, statement_expr, "GlobalStateTransformConjecture expression should have length 2")
                rhs = self._mk_term(statement_expr[1], None, parent_action, None, statement_expr)
                rv = FVRequirement(rhs)

            elif statement_expr[0] == 'local':
                self.assertOrSyntaxError(len(statement_expr) == 6, statement_expr, 'Local var dec should have form (local name : type = term) or := instead of =')
                self.assertOrSyntaxError(statement_expr[2] == ':' and (statement_expr[4] == ":=" or statement_expr[4] == "="), statement_expr,
                                         'Local var dec should have form (local name : type = term)  or := instead of =')
                sort = self._mk_sort(statement_expr[3])
                rhs = self._mk_term(statement_expr[5], None, parent_action, None, statement_expr)
                varname = castid(LocalVarId, statement_expr[1])
                self.top.register_sorted_name(varname,sort)
                lvd = LocalVarDec(varname, rhs, sort)
                if varname in parent_action.local_vars:
                    self.syntaxError(statement_expr, "Redeclaration of local variable")
                parent_action.local_vars[varname] = lvd
                rv = lvd

            elif statement_expr[0] == 'if':
                test = self._mk_term(statement_expr[1], None, parent_action, None, statement_expr)
                self.assertOrSyntaxError(isinstance(statement_expr[2], SExpr) and isinstance(statement_expr[4], SExpr), statement_expr)
                rv = IfElse(test,[],[])
                f"Expected {statement_expr} to have the form `(if TEST (BLOCK) else (BLOCK))`"
                rv.true_branch = self._mk_statements(statement_expr[2], parent_action, rv)
                self.assertOrSyntaxError(statement_expr[3] == 'else', statement_expr), \
                f"Expected {statement_expr} to have the form `(if TEST (BLOCK) else (BLOCK))`"
                rv.false_branch = self._mk_statements(statement_expr[4], parent_action, rv)

            else:
                self.assertOrSyntaxError(len(statement_expr) == 3, statement_expr,
                                         "As of 16 Mar 2018, every code block statement other than a conjecture or local var intro should be"
                                         "a triple: a := (or =), +=, -=, *= specifically. See\n" + str(statement_expr))
                assert statement_expr.coord() is not None
                rhs = self._mk_term(statement_expr[2], None, parent_action, None, statement_expr)
                # print(statement_expr.coord, rhs.coord, statement_expr[2])
                assert rhs.coord is not None, f"{rhs} has no FileCoord. it's a {type(rhs)}"
                varname = castid(StateVarId, statement_expr[0])
                # TODO: for requiring primed variables. recall, though, that this makes += syntax kinda odd.
                assert isprimed(varname), f"Replace assigned-to state var name {varname} with {primed(varname)}."
                unprimed_name = unprimed(varname)

                # self.assertOrSyntaxError(isprimed(varname), statement, f"To assign to a state variable {varname}, you must assign to {primed(varname)}, which indicates \"the next value of X\".")
                self.assertOrSyntaxError(unprimed_name in self.top.state_var_decs, statement_expr, f"{unprimed_name} not recognized as a state variable. State variables are: " + str(self.top.state_var_decs.keys()))
                vardec = self.top.state_var_decs[unprimed_name]
                orig : Statement
                reduced : Statement

                if statement_expr[1] == ':=' or statement_expr[1] == "=":
                    rv = StateVarAssign(vardec, rhs)
                else:
                    assert statement_expr.coord is not None
                    var = self.top.new_state_var_ref(unprimed_name, statement_expr.coord())
                    orig = StateVarAssign(vardec, rhs, statement_expr[1])
                    if orig.varop == "+=":
                        reduced = StateVarAssign(vardec, FnApp('+', [var, rhs], rhs.coord))
                    elif orig.varop == '-=':
                        reduced = StateVarAssign(vardec, FnApp('-', [var, rhs], rhs.coord))
                    elif orig.varop == '*=':
                        reduced = StateVarAssign(vardec, FnApp('*', [var, rhs], rhs.coord))
                    else:
                        raise Exception
                    reduced.orig = orig
                    rv = reduced

            rv.parent_block = parent_block
            rv.grandparent_ifelse = parent_ifelse
            return rv

        except Exception as e:
            logging.error(f"Problem with {statement_expr}")
            raise e

    @staticmethod
    def mk_literal(x:str, parent_SExpr:Optional[SExpr] = None, prog:Optional[L4Contract] = None) -> Term:
        coord = FileCoord(parent_SExpr.line, parent_SExpr.col) if parent_SExpr else None
        if isInt(x):
            return IntLit(int(x), coord)
        if isFloat(x):
            return FloatLit(float(x), coord)
        if x == 'false':
            return BoolLit(False, coord)
        if x == 'true':
            return BoolLit(True, coord)
        # if x == 'never':
        #     return DeadlineLit(x, coord)
        if x[-1].lower() in SUPPORTED_TIMEUNITS and isInt(x[:-1]):
            rv = SimpleTimeDeltaLit(int(x[:-1]), x[-1].lower(), coord)
            # print('STD', rv)
            return rv
        if prog and x in prog.roles:
            return RoleIdLit(x)

        syntaxErrorX(parent_SExpr, f"Don't recognize name {x}")


    """
    parent_SExpr is used for debugging since s-expressions carry their original line number. not used for anything else. 
    """
    def _mk_term(self, x:Union[str, SExpr],
                 parent_situation : Optional[Situation] = None,
                 parent_action : Optional[Action] = None,
                 parent_action_rule : Optional[ActionRule] = None,
                 parent_SExpr : Optional[SExpr] = None ) -> Term:
        assert parent_SExpr is not None, x # todo clean this up
        assert x != "no_time_constraint"

        if isinstance(x,str):
            if x in UNICODE_TO_ASCII:
                x = UNICODE_TO_ASCII[x]
            if x in EXEC_ENV_VARIABLES:
                if x == "next_event_td":
                    self.assertOrSyntaxError(self._building_next_action_rule, parent_SExpr, "Can't use next_event_td when not in the scope of an action rule.")

                # elif x == "last_event_td":
                #     self.assertOrSyntaxError(self._building_action_id is not None, parent_SExpr, "Can't use last_event_td when not in the scope of an action.")
                #     self.assertOrSyntaxError(not self._building_action_rule, parent_SExpr, ("last_event_td directly within the time constraint or `where` clause of a next-action rule is not supported, because it's confusing." +
                #                                                                             "Use last_situation_td instead."))
                # elif x == "last_situation_td":
                #     self.assertOrSyntaxError(self._building_situation_id is not None, parent_SExpr, "Can't use last_situation_td when not in the scope of a situation.")

                return FnApp(x,[], parent_SExpr.coord() if parent_SExpr else None)

            if x in TIME_CONSTRAINT_KEYWORDS:
                assert x != "no_time_constraint"
                if x == "immediately":

                    #  SExpr(['==', 'next_event_td',
                    #                   SExpr(['+', "last_situation_td", "1" + timeunit], sexpr2.line,
                    #                         sexpr2.col)], sexpr2.line, sexpr2.col)
                    coord = parent_SExpr.coord()
                    return FnApp('==', [
                                FnApp('next_event_td',[],coord),
                                FnApp('last_situation_td', [], coord)
                                # FnApp('+', [
                                #     FnApp('last_situation_td',[],coord),
                                #     SimpleTimeDeltaLit(1, self.top.timeunit, coord)
                                # ], coord)
                            ], coord)
                # elif x == "no_time_constraint":
                #     return None
                raise Exception
                # return DeadlineLit(x)

            if x in self.top.state_var_decs:
                return StateVar(self.top.state_var_decs[cast(StateVarId, x)], parent_SExpr.coord() if parent_SExpr else None)

            if isprimed(x):
                if not self._building_future_action_rule:
                    self.assertOrSyntaxError(self._building_situation_id is None and not self._building_next_action_rule,
                                             parent_SExpr,
                                             f"Can't use primed variables outside of a StateTransform or Future section.")# {self._building_situation_id} {self._building_next_action_rule}")
                self.assertOrSyntaxError(unprimed(x) in self.top.state_var_decs, parent_SExpr, f"Primed variable {x} does not appear to be a state variable.")
                return PrimedStateVar(self.top.state_var_decs[unprimed(x)], parent_SExpr.coord() if parent_SExpr else None)

            # if parent_action and (x in parent_action.local_vars):
            #     return LocalVar(parent_action.local_vars[cast(LocalVarId,x)])

            if x in self.top.contract_params:
                return ContractParam(self.top.contract_params[cast(ContractParamId,x)], parent_SExpr.coord() if parent_SExpr else None)

            # print("parent_action_rule", parent_action_rule)
            # if x == 'order' and parent_action_rule:
            #     print("args", parent_action_rule.args)
            if parent_action_rule and parent_action_rule.ruleparam_names and x in parent_action_rule.ruleparam_names:
                assert parent_SExpr is not None and parent_SExpr.coord() is not None
                assert parent_action_rule.ruleparam_to_ind is not None
                return RuleBoundActionParam(cast(RuleParamId, x), parent_action_rule,
                                            parent_action_rule.ruleparam_to_ind[castid(RuleParamId, x)],
                                            parent_SExpr.coord())

            if parent_action and x in parent_action.param_sorts_by_name:
                assert parent_SExpr is not None and parent_SExpr.coord() is not None
                assert parent_action.param_name_to_ind is not None
                return ActionBoundActionParam(cast(ActionParamId, x), parent_action,
                                              parent_action.param_name_to_ind[castid(ActionParamId,x)],
                                              parent_SExpr.coord())

            if x in self.top.definitions:
                return self._mk_term(self.top.definitions[castid(DefinitionId, x)].body,
                                     parent_situation, parent_action, parent_action_rule,
                                     parent_SExpr)

            if parent_action and x in parent_action.local_vars:
                return LocalVar(parent_action.local_vars[castid(LocalVarId, x)],
                                parent_SExpr.coord() if parent_SExpr else None)

            return L4ContractConstructor.mk_literal(x, parent_SExpr, self.top)

        else: # SExpr
            if self._is_macro_app(x):
                x = self.handle_apply_macro(x)

            pair = try_parse_as_fn_app(x)
            if pair:
                fnsymb_name = pair[0]
                # fnsymb_or_name = cast(Union[str,FnSymb], self.top.fnsymbs[fnsymb_name] if fnsymb_name in self.top.fnsymbs else fnsymb_name)
                self.top.fnsymb_names.add(fnsymb_name)
                args : List[Term]

                if fnsymb_name in ('cast','check','trust','units'):
                    args = [cast(Term, self.mk_sort_lit(pair[1][0]))] + [self._mk_term(arg, parent_situation, parent_action, parent_action_rule, x) for arg in pair[1][1:]]

                elif fnsymb_name == "str2dt":
                    assert isinstance(pair[1],SExpr) and isinstance(pair[1][0], SExpr) and pair[1][0][0] == STRING_LITERAL_MARKER, pair
                    try:
                        # pair[1] is the args tuple to str2dt, pair[1][0] is the first and only arg, and STRLIT expression, and
                        # pair[1][0][1] is the date string itself
                        dt = dateutil.parser.parse(pair[1][0][1])
                        # return DateTimeLit(dt, pair[1][0].coord())
                        self.assertOrSyntaxError(self.top.start_datetime is not None, x)
                        if self.top.start_datetime: # redundant None check for mypy
                            return TimeDeltaLit(dt - self.top.start_datetime, x.coord())
                    except Exception as e:
                        print(e)
                        raise e
                else:
                    args = [ self._mk_term(arg, parent_situation, parent_action, parent_action_rule, x) for arg in pair[1] ]
                return FnApp(
                    fnsymb_name,
                    args,
                    x.coord()
                )
            else:
                if x in EXEC_ENV_VARIABLES:
                    self.syntaxError(x, f"You're using environment variable {x} like a 0-arg function symbol. Remove the brackets please.")
                elif x[0] in self.top.sorts:
                    return FnApp( "units", [
                        self.mk_sort_lit(x[0]),
                        self._mk_term(x[1], parent_situation, parent_action, parent_action_rule, parent_SExpr),
                    ], x.coord())
                elif x[0] in self.top.sort_definitions:
                    return FnApp("units", [
                        self.mk_sort_lit(x[0]),
                        self._mk_term(x[1], parent_situation, parent_action, parent_action_rule, parent_SExpr)
                    ], x.coord())
                else:
                    if x[0] in INFIX_FN_SYMBOLS:
                        self.syntaxError(x, f"Didn't recognize symbol {x[0]} in: {x}. Did you mean to use infix notation?")
                    else:
                        self.syntaxError(x, f"Didn't recognize symbol {x[0]} in: {x}")

                assert False # this is just to get mypy to not complain about missing return statement

    def _mk_time_constraint(self, expr:SExprOrStr, src_situation:Optional[Situation], src_action:Optional[Action],
                            parent_action_rule:Optional[ActionRule], parent_sexpr:SExpr) -> Optional[Term]:
        rv : Term
        # if expr in TIME_CONSTRAINT_KEYWORDS:
        #     return self._mk_term(expr, src_situation, src_action)
        # elif isinstance(expr,str):
        #     self.syntaxError(expr, f"Unrecognized token {expr} in time constraint keyword position.")
        if isinstance(expr,str):
            if expr == "no_time_constraint":
                return None
            rv = self._mk_term(expr, src_situation, src_action, parent_action_rule, parent_sexpr)
        else:
            self.assertOrSyntaxError( len(expr) > 1, expr)
            pair = try_parse_as_fn_app(expr)
            if pair and pair[0] in TIME_CONSTRAINT_PREDICATES:
                # return self._mk_term(expr, src_situation, src_action, parent_action_rule, None)
                rv = FnApp(
                        pair[0],
                        [self._mk_term(arg, src_situation, src_action, parent_action_rule, expr) for arg in pair[1]],
                        FileCoord(expr.line, expr.col)
                    )
            else:
                if src_situation:
                    print("pair: ", pair)
                    self.syntaxError(expr, f"Unhandled time constraint predicate {expr} in situation {src_situation.situation_id}")
                elif src_action:
                    self.syntaxError(expr, f"Unhandled time constraint predicate {expr} in action {src_action.action_id}")

                # raise Exception("Must have time constraint. You can use `immediately` or `no_time_constraint` or `discretionary`")
                return None

        if not (isinstance(rv,FnApp) and rv.fnsymb_name in ["≤","<=","<"] and isinstance(rv.args[0],FnApp) and rv.args[0].fnsymb_name == "next_event_td"):   # type:ignore
            if self.verbose:
                print("Atypical time constraint:", rv)
        # else:
        #     print("Typical time constraint:", rv)
        return rv

    def _mk_future_action_rule(self, expr:SExpr, src_action:Action) -> PartyFutureActionRule:
        self._building_future_action_rule = True
        entrance_enabled_guard: Optional[Term] = None
        if expr[0] == 'if':
            entrance_enabled_guard = self._mk_term(expr[1], None, src_action)
            expr = expr[2]

        role_id = castid(RoleId, expr[0])
        deontic_keyword = castid(DeonticKeyword, expr[1])
        args: Optional[List[RuleParamId]] = None
        if isinstance(expr[2],str):
            action_id = castid(ActionId,expr[2])
            args = []
        else:
            action_id = castid(ActionId, (expr[2][0]))

            args_part = expr[2].tillEnd(1)
            if len(args_part) == 0:
                args = []
            elif args_part[0][0] == "?":
                assert all([args_part[i][0] == "?" for i in range(len(args_part))]), \
                    "Either all or none of the action argument positions in an action rule must be newly-bound variables prefixed with '?'."
                args = cast(List[RuleParamId], args_part)

        if not is_derived_trigger_id(action_id):
            self.referenced_nonderived_action_ids.add(action_id)

        rv = PartyFutureActionRule(src_action.action_id, [role_id], action_id, args, entrance_enabled_guard, deontic_keyword)
        if args is None:
            rv.fixed_args = [self._mk_term(arg, None, src_action, rv, args_part) for arg in args_part]

        rem = expr.tillEnd(3)
        self._handle_optional_action_rule_parts(rem, rv, None, src_action)

        src_action.add_future_action_rule(rv)
        self._building_future_action_rule = False
        return rv


    def _mk_next_action_rule(self, expr:SExpr, src_situation:Situation, parent_action:Optional[Action]) -> None:
        self._building_next_action_rule = True
        entrance_enabled_guard: Optional[Term] = None
        if expr[0] == 'if':
            entrance_enabled_guard = self._mk_term(expr[1], src_situation, parent_action, None, expr)
            if len(expr) == 3:
                expr = expr[2]
            else:
                # multiple rules sharing an enabled-guard
                for unguarded_rule in expr[2:]:
                    self._mk_next_action_rule(SExpr([expr[0], expr[1], unguarded_rule], expr.line, expr.col, expr.symb), src_situation, parent_action)
                return

        deontic_keyword : Optional[str] = None
        role_id : RoleId
        role_ids: List[RoleId]
        action_id : ActionId
        ruleparams : Optional[List[RuleParamId]] = None
        nar : NextActionRule
        if len(expr) == 2:
            if isinstance(expr[0],str):
                action_id = castid(ActionId,expr[0])
                ruleparams = []
            else:
                action_id = castid(ActionId, (expr[0][0]))
                ruleparams_part = expr[0].tillEnd(1)
                if len(ruleparams_part) == 0:
                    ruleparams = []
                elif ruleparams_part[0][0] == "?":
                    assert all([ruleparams_part[i][0] == "?" for i in range(len(ruleparams_part))]), \
                        "Either all or none of the action argument positions in an action rule must be newly-bound variables prefixed with '?'."
                    ruleparams = cast(List[RuleParamId], ruleparams_part)

            if not is_derived_trigger_id(action_id):
                self.referenced_nonderived_action_ids.add(action_id)

            nar = EnvNextActionRule(src_situation.situation_id, action_id, ruleparams, entrance_enabled_guard)
            rem = expr.tillEnd(1)

        else:
            role_ids = [castid(RoleId, expr[0])] if (isinstance(expr[0],Literal) or isinstance(expr[0],str)) else expr[0]
            deontic_keyword = castid(DeonticKeyword, expr[1])
            self.assertOrSyntaxError(deontic_keyword in DEONTIC_KEYWORDS, expr, deontic_keyword)
            if isinstance(expr[2],str):
                action_id = castid(ActionId,expr[2])
                ruleparams = []
            else:
                action_id = castid(ActionId, (expr[2][0]))
                ruleparams_part = expr[2].tillEnd(1)
                if len(ruleparams_part) == 0:
                    ruleparams = []
                elif ruleparams_part[0][0] == "?":
                    assert all([ruleparams_part[i][0] == "?" for i in range(len(ruleparams_part))]), \
                        "Either all or none of the action argument positions in an action rule must be newly-bound variables prefixed with '?'."
                    ruleparams = cast(List[RuleParamId], ruleparams_part)

            if not is_derived_trigger_id(action_id):
                self.referenced_nonderived_action_ids.add(action_id)

            nar = PartyNextActionRule(src_situation.situation_id, role_ids, action_id, ruleparams, entrance_enabled_guard, deontic_keyword)
            rem = expr.tillEnd(3)

        if ruleparams is None:
            nar.fixed_args = [self._mk_term(arg, src_situation, parent_action, nar, ruleparams_part) for arg in ruleparams_part]

        self._handle_optional_action_rule_parts(rem, nar, src_situation, parent_action)
        if deontic_keyword == "must" and nar.time_constraint:
            if isinstance(nar.time_constraint, Literal) and nar.time_constraint.lit == "no_time_constraint":
                assert False, nar

        assert not nar.fixed_args or not nar.where_clause
        src_situation.add_action_rule(nar)
        self._building_next_action_rule = False

    def _handle_optional_action_rule_parts(self, rem:SExpr, ar:ActionRule, src_situation:Optional[Situation], src_or_parent_act: Optional[Action]):
        found_labeled_time_constraint = False
        for x in rem:
            if not isinstance(x, str):
                if x[0] == "where":
                    ar.where_clause = self._mk_term(x[1], src_situation, src_or_parent_act, ar, rem)
                elif x[0] == "when":
                    found_labeled_time_constraint = True
                    if len(x) > 2:
                        ar.time_constraint = self._mk_time_constraint(x.tillEnd(1), src_situation, src_or_parent_act, ar, x)
                    else:
                        ar.time_constraint = self._mk_time_constraint(x[1], src_situation, src_or_parent_act, ar, x)

                elif x[0] in {"before", "within", "at", "after", "at_or_after"}:
                    found_labeled_time_constraint = True
                    rest = x[1] if len(x) == 2 else x.tillEnd(1)
                    if x[0] == "within":
                        symb = '≤'
                    elif x[0] == "at":
                        symb = '=='
                    elif x[0] == "before":
                        symb = '<'
                    elif x[0] == "after":
                        symb = '>'
                    elif x[0] == "at_or_after":
                        symb = '≥'
                    expanded = x.newHere([symb, 'next_event_td', rest])
                    ar.time_constraint = self._mk_time_constraint(expanded, src_situation, src_or_parent_act, ar, x)
                    cast(Term, ar.time_constraint).src_expr = x

                # =============TimeDelta since last shorthand=============
                elif x[0] in { "before_split", "within_split", "at_split", "after_split", "at_or_after_split"}:
                    found_labeled_time_constraint = True
                    rest = x[1] if len(x) == 2 else x.tillEnd(1)
                    if x[0] == "within_split":
                        symb = '≤'
                    elif x[0] == "at_split":
                        symb = '=='
                    elif x[0] == "before_split":
                        symb = '<'
                    elif x[0] == "after_split":
                        symb = '>'
                    elif x[0] == "at_or_after_split":
                        symb = '≥'
                    expanded = x.newHere([symb, 'next_event_td', x.newHere(['+', 'last_situation_td', rest])])
                    ar.time_constraint = self._mk_time_constraint(expanded, src_situation, src_or_parent_act, ar, x)
                    cast(Term, ar.time_constraint).src_expr = x

                # =============DateTime shorthand=============
                elif x[0] in {"before_dt", "by", "within_dt", "on", "at_dt", "after_dt", "at_or_after_dt"}:
                    found_labeled_time_constraint = True
                    rest = x[1] if len(x) == 2 else x.tillEnd(1)
                    if x[0] == "on" or x[0] == "at_dt":
                        symb = '=='
                    elif x[0] == "within_dt" or x[0] == "by":
                        symb = '≤'
                    elif x[0] == "before_dt":
                        symb = '<'
                    elif x[0] == 'after_dt':
                        symb = '>'
                    elif x[0] == 'at_or_after_dt':
                        symb = '≥'
                    expanded = SExpr(['≤', 'next_event_dt', rest], x.line, x.col)
                    ar.time_constraint = self._mk_time_constraint(expanded, src_situation, src_or_parent_act, ar, x)
                    cast(Term,ar.time_constraint).src_expr = x

                else:
                    self.syntaxError(rem)



            elif x in TIME_CONSTRAINT_KEYWORDS:
                found_labeled_time_constraint = True
                ar.time_constraint = self._mk_time_constraint(x, src_situation, src_or_parent_act, ar, rem)

            # else:
            #     self.syntaxError(rem, "wtf is this? " + str(x))

        if not found_labeled_time_constraint:
            assert ar.time_constraint is None
            # ar.time_constraint = self._mk_time_constraint("no_time_constraint", src_situation, src_or_parent_act, ar, rem)
            # assert ar.time_constraint is not None, f"Currently a time constraint is needed in the S-Expr syntax, but it can be 'no_time_constraint'. See {str(rem)}"




def try_parse_as_fn_app(x:SExpr)  -> Optional[Tuple[str, SExpr]]:
    return maybe_as_infix_fn_app(x) or maybe_as_prefix_fn_app(x) or maybe_as_postfix_fn_app(x)


def maybe_as_prefix_fn_app(se:SExpr) -> Optional[Tuple[str, SExpr]]:
    if isinstance(se[0],str):
        symb = se[0]
        if symb in PREFIX_FN_SYMBOLS or symb in INFIX_FN_SYMBOLS:
            return symb, se.tillEnd(1)
    return None

def maybe_as_infix_fn_app(se:SExpr) -> Optional[Tuple[str, SExpr]]:
    if len(se) == 3 and isinstance(se[1],str):
        symb : str = se[1]
        if symb in INFIX_FN_SYMBOLS:
            return symb, se.withElementDropped(1)
    return None

def maybe_as_postfix_fn_app(se:SExpr) -> Optional[Tuple[str, SExpr]]:
    if isinstance(se[-1],str):
        symb = se[-1]
        if symb in POSTFIX_FN_SYMBOLS:
            return symb, se.fromStartToExclusive(len(se) - 1)
    return None


