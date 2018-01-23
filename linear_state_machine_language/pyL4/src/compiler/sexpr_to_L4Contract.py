import logging
from mypy_extensions import NoReturn

from src.model.ActionRule import FutureActionRuleType, PartyFutureActionRule, ActionRule, NextActionRule, EnvNextActionRule, \
    PartyNextActionRule
from src.model.Section import Section
from src.independent.typing_imports import *
from src.constants_and_defined_types import *
from src.model.Term import Term
from src.model.Action import Action
from src.model.ContractClaim import ContractClaim
from src.model.ContractParamDec import ContractParamDec
from src.model.Definition import Definition
from src.model.GlobalVarDec import GlobalVarDec
from src.model.Sort import Sort, NonatomicSort

from src.independent.FileCoord import FileCoord
from src.correctness_checks import L4ContractConstructorInterface
from src.independent.SExpr import SExpr, SExprOrStr
from src.independent.parse_sexpr import castse, STRING_LITERAL_MARKER
from src.model.BoundVar import ContractParam, RuleBoundActionParam, ActionBoundActionParam, \
    StateTransformLocalVar, GlobalVar
from src.model.GlobalStateTransform import GlobalStateTransform
from src.model.GlobalStateTransformStatement import GlobalStateTransformStatement, InCodeConjectureStatement, \
    StateTransformLocalVarDec, IfElse, GlobalVarAssignStatement, IncrementStatement, DecrementStatement, \
    TimesEqualsStatement
from src.model.L4Contract import L4Contract, is_derived_destination_id, is_derived_trigger_id, \
    derived_trigger_id_to_section_id, derived_destination_id
from src.model.L4Macro import L4Macro
from src.model.Literal import SortLit, IntLit, FloatLit, BoolLit, DeadlineLit, SimpleTimeDeltaLit
from src.model.Term import FnApp
from src.temp_src.until_sort_duplicates_implemented import temp_normalize_sort
from src.util import streqci, chcaststr, isFloat, isInt, todo_once, castid, chcast


class L4ContractConstructor(L4ContractConstructorInterface):
    def __init__(self, filename:Optional[str] = None) -> None:
        self.top : L4Contract = L4Contract(filename or '')
        self.referenced_nonderived_section_ids: Set[SectionId] = set()
        self.referenced_nonderived_action_ids: Set[ActionId] = set()
        self.after_model_build_requirements : List[Tuple[Callable[[],bool],str]] = []

    def addAfterBuildAssertion(self, f:Callable[[],bool], errmsg:str):
        self.after_model_build_requirements.append((f,errmsg))


    def syntaxError(self, expr: SExprOrStr, msg:Optional[str] = None) -> NoReturn:
        if isinstance(expr,SExpr):
            raise SyntaxError((msg if msg else "") +
                              "\n" + str(expr) +
                              "\nline " + str(expr.line) +
                              "\n" + str(self.top.filename))
        else:
            raise SyntaxError((msg if msg else "") +
                              "\n" + str(self.top.filename))

    def assertOrSyntaxError(self, test:bool, expr:SExpr, msg:Optional[str] = None) -> Union[NoReturn,None]:
        if not test:
            self.syntaxError(expr, msg)
        return None

    @staticmethod
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

    @staticmethod
    def assertOrSyntaxErrorX(test:bool, expr:Optional[SExpr], msg:Optional[str] = None) -> Union[NoReturn,None]:
        if not test:
            L4ContractConstructor.syntaxErrorX(expr, msg)
        return None

    def mk_l4contract(self, l:List[SExpr]) -> L4Contract:
        x : SExpr
        for x in l:
            #assert len(x) >= 2, "Problem top-level: " + str(x)
            rem = x.tillEnd(1)
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if   head(MACRO_DEC_LABEL):
                macroname = chcaststr(x[1])
                macroparams : List[str]
                if isinstance(x[2],str):
                    macroparams = [x[2]]
                else:
                    macroparams = cast(List[str],castse(x[2]).lst)
                macrobody = chcast(SExpr, x[3])
                self.top.macros[ macroname] = L4Macro(macroparams, macrobody)

            elif   head(GLOBAL_VARS_AREA_LABEL):
                self.top.global_var_decs = self._mk_global_vars(rem)

            elif head(CONTRACT_PARAMETERS_AREA_LABEL):
                # assert all(isinstance(expr[0],str) for expr in rem)
                self.top.contract_params = {castid(ContractParamId,expr[0]) : self._mk_contract_param(expr) for expr in rem}

            elif head(ROLES_DEC_LABEL):
                self.top.roles.extend(self._mk_actors(rem))

            elif head(PROSE_CONTRACT_AREA_LABEL):
                self.top.prose_contract = self._mk_prose_contract(cast(List[List[str]], rem))

            elif head(FORMAL_CONTRACT_AREA_LABEL):
                self._mk_main_program_area(rem)

            elif head(TIMEUNIT_DEC_LABEL):
                self.top.timeunit = chcaststr(x[1]).lower()
                self.assertOrSyntaxError( self.top.timeunit in SUPPORTED_TIMEUNITS, x, f"{TIMEUNIT_DEC_LABEL} must be one of {str(SUPPORTED_TIMEUNITS)}")

            elif head(SORT_DEFINITIONS_AREA):
                self.top.sort_definitions = self._mk_sort_definitions(rem)

            elif head(DEFINITIONS_AREA):
                self.top.definitions = self._mk_definitions(rem)

            elif head(TOPLEVEL_CLAIMS_AREA_LABEL):
                self.top.claims = self._mk_claims(rem)

            elif head("VerificationDefinition"):
                continue

            elif head( DOT_FILE_NAME_LABEL ):
                self.top.dot_file_name = chcaststr(x[1][1]) # the extra [1] is because its parse is of the form ['STRLIT', 'filename']
            elif head( IMG_FILE_NAME_LABEL ):
                self.top.img_file_name = chcaststr(x[1][1]) # the extra [1] is because its parse is of the form ['STRLIT', 'filename']

            else:
                raise Exception("Unsupported: ", x[0])

        for f in self.after_model_build_requirements:
            if not f[0]():
                raise Exception(f[1])

        # floating_rules_transpile_away(self.top)
        return self.top

    def _mk_sort(self, x:SExprOrStr) -> Sort:
        sort: Sort
        if isinstance(x,str):
            sort = temp_normalize_sort(castid(SortId, x))
        else:
            if len(x) == 2 and x[0] == STRING_LITERAL_MARKER:
                # This is just a sort that has brackets/parentheses/spaces in it, so that it has to be
                # enclosed in quotes in source code.
                sort = self._mk_sort(x[1])
            else:
                assert len(x) >= 2
                sort = NonatomicSort(x[0], tuple(self._mk_sort(x[i]) for i in range(1,len(x))))
        self.top.sorts.add(sort)
        assert sort is not None
        return sort

    def _mk_sort_lit(self,x:SExprOrStr) -> SortLit:
        if isinstance(x,str):
            return SortLit(temp_normalize_sort(x))
        else:
            assert len(x) == 2 and x[0] == STRING_LITERAL_MARKER
            # assert x[1] in AllSorts
            return SortLit(x[1])

    def _mk_contract_param(self, expr) -> ContractParamDec:
        self.assertOrSyntaxError( len(expr) == 5, expr, "Contract parameter dec should have form (name : sort := term)" )
        sort = self._mk_sort(expr[2])
        return ContractParamDec(expr[0], sort, self._mk_term(expr[4]))

    def _mk_global_vars(self, l:SExpr) -> Dict[GlobalVarId, GlobalVarDec]:
        rv : Dict[GlobalVarId, GlobalVarDec] = dict()
        for dec in l:
            try:
                i = 0
                modifiers : List[str] = []
                while True:
                    if dec[i] in VARIABLE_MODIFIERS:
                        modifiers.append(chcaststr(dec[i]))
                        i += 1
                    else:
                        break
                name = cast(GlobalVarId, chcaststr(dec[i]))
                sort = self._mk_sort(dec[i + 2])

                initval : Optional[Term] = None
                if i+3 < len(dec) and (dec[i+3] == ':=' or dec[i+3] == '='):
                    initval = self._mk_term(dec[i + 4])

                # print("sort: ", str(sort))
                # print("initval: ", str(initval), type(initval))
                rv[name] = GlobalVarDec(name, sort, initval, modifiers)
            except Exception as e:
                logging.error("Problem processing " + str(dec))
                raise e

        # logging.info(str(rv))
        return rv

    def _mk_claims(self, l:SExpr) -> List[ContractClaim]:
        # rv = [ContractClaim(self.term(x)) for x in l]
        rv = [ContractClaim(x) for x in l]
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

    def _mk_sort_definitions(self, s:SExpr) -> Dict[DefinitionId, Sort]:
        self.assertOrSyntaxError(all(len(x) == 3 for x in s), s,
                                 "Definition declaration S-expressions should have the form (id = SExpr) or (id := SExpr)")
        return {x[0] : self._mk_sort(x[2]) for x in s}

    def _mk_prose_contract(self, l: List[List[str]]) -> ProseContract:
        rv = {castid(ProseClauseId,x[0]): x[1] for x in l}
        # logging.info(str(rv))
        return rv

    def handle_apply_macro(self, x:SExpr) -> SExpr:
        macroname = chcaststr(x[1])
        macro : L4Macro = self.top.macros[macroname]
        if isinstance(x[2],str):
            return macro.subst([x[2]])
        else:
            return macro.subst(x[2])


    def _mk_main_program_area(self, l:SExpr) -> None:
        self.assertOrSyntaxError(l[0][0] == STRING_LITERAL_MARKER, l[0], f"Immediately after the {FORMAL_CONTRACT_AREA_LABEL} keyword should be a string literal that gives the contract's name")
        self.top.contract_name = chcaststr(l[0][1]) # [1] because STRLIT sexpr
        x: SExpr

        for x in l[1:]:
            assert len(x) >= 2
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if  head(APPLY_MACRO_LABEL):
                x = self.handle_apply_macro(x)

            if head(START_SECTION_LABEL):
                self.assertOrSyntaxError( len(x) == 2, l, "StartState declaration S-expression should have length 2")
                section_id = cast(SectionId, chcaststr(x[1]))
                self.top.start_section_id = section_id
                if not is_derived_destination_id(section_id):
                    self.referenced_nonderived_section_ids.add(section_id)

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

            elif head(SECTION_LABEL):
                section_id = cast(SectionId, chcaststr(x[1]))
                section_data = x.tillEnd(2)
                section = self._mk_section(section_id, section_data, None)
                self.top.sections_by_id[section_id] = section
                self.top.ordered_declarations.append(section)

            else:
                self.syntaxError(l, f"Unrecognized head {x[0]}")


    def _mk_section(self, section_id:SectionId, rest:SExpr, parent_action:Optional[Action]) -> Section:
        section = Section(section_id)
        x: SExpr
        for x in rest:
            assert isinstance(x,SExpr), f"{x} should be an s-expression"

            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if head(SECTION_PRECONDITION_LABEL):
                section.preconditions.append(self._mk_term(x[1], section, parent_action))

            elif head(SECTION_DESCRIPTION_LABEL):
                section.section_description = chcaststr(x[1][1]) # extract from STRLIT expression

            elif head(OUT_CONNECTIONS_LABEL):
                if isinstance(x[1],SExpr) and isinstance(x[1][0],str) and (x[1][0] == 'guardsDisjointExhaustive' or x[1][0] == 'timeConstraintsPartitionFuture'):
                    x = x[1]
                    todo_once("guardsDisjointExhaustive etc in section()")
                action_rule_exprs = castse(x.tillEnd(1))
                for action_rule_expr in action_rule_exprs:
                    if action_rule_expr[0] == APPLY_MACRO_LABEL:
                        action_rule_expr = self.handle_apply_macro(action_rule_expr)

                    self._mk_next_action_rule(action_rule_expr, section, parent_action)

            elif head(PROSE_REFS_LABEL):
                section.prose_refs = cast(List,x[1:]).copy()

            elif 'visits' in x or 'traversals' in x:
                section.visit_bounds = x # self.term(x, None, section)

            elif head("possibly-from-earlier"):
                assert x[1] in self.top.roles
                assert x[2] in ["must-later", "may-later"]
                floating_rule_type = FutureActionRuleType(x[1], x[3], x[2])
                section.possible_floating_rule_types.add(floating_rule_type)
                self.top.possible_floating_rule_types.add(floating_rule_type)

            else:
                self.syntaxError(x, f"Unsupported declaration type {x[0]} in section {section_id}")
                todo_once(f"Handle {x[0]} in section dec")

        return section

    def _mk_action(self, action_id:ActionId, params_sexpr:Optional[List[List[str]]], rest:SExpr) -> Action:
        a = Action(action_id)
        dest_section_id = None
        x: SExpr
        if params_sexpr is not None: #isinstance(params_sexpr, SExpr):
            a.param_sorts_by_name = self._mk_action_params(params_sexpr)
            a.param_names = [castid(ActionBoundActionParamId, y[0]) for y in params_sexpr]
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
                a.global_state_transform= self._mk_global_state_transform(cast(List[SExpr], x.tillEnd(1).lst), a)

            elif head(PROSE_REFS_LABEL):
                a.prose_refs  = cast(List[str], x.tillEnd(1).lst)

            elif head(TRANSITIONS_TO_LABEL):
                dest_section_id = x[1]
                if not is_derived_destination_id(dest_section_id) and dest_section_id != LOOP_KEYWORD:
                    self.referenced_nonderived_section_ids.add(dest_section_id)

            elif 'traversals' in x or 'visits' in x:
                a.traversal_bounds = x # self.term(x, None, a)

            elif head('Future'):
                a.futures = self._mk_futures(x.tillEnd(1), a)

            elif head(ALLOWED_SUBJECTS_DEC_LABEL):
                a.allowed_subjects = x.tillEnd(1)

            elif head(FOLLOWING_SECTION_DEC_LABEL):
                anon_sect_id : str
                if is_derived_trigger_id(action_id):
                    anon_sect_id = derived_trigger_id_to_section_id(action_id)
                else:
                    anon_sect_id = derived_destination_id(action_id)
                a.following_anon_section  = self._mk_section(anon_sect_id, x.tillEnd(1), a)
                a.following_anon_section.parent_action_id = action_id
                self.top.sections_by_id[a.following_anon_section.section_id] = a.following_anon_section

            else:
                todo_once(f"Handle {x[0]} in action dec.")

        if dest_section_id:
            a.dest_section_id = dest_section_id
        else:
            if is_derived_trigger_id(a.action_id):
                a.dest_section_id = derived_trigger_id_to_section_id(a.action_id)
                self.referenced_nonderived_section_ids.add(a.dest_section_id)
            else:
                a.dest_section_id = derived_destination_id(a.action_id)

        return a

    def _mk_futures(self, rules:SExpr, src_action:Action) -> List[PartyFutureActionRule]:
        rv : List[PartyFutureActionRule] = []
        for action_rule_expr in rules:
            if action_rule_expr[0] == APPLY_MACRO_LABEL:
                action_rule_expr = self.handle_apply_macro(action_rule_expr)

            rv.append( self._mk_future_action_rule(action_rule_expr, src_action) )
        return rv

    def _mk_action_params(self, parts:List[List[str]]) -> ParamsDec:
        pdec : List[str]
        rv : ParamsDec = dict()
        for pdec in parts:
            assert len(pdec) == 3, f"Expected [<param name str>, ':', SORTstr] but got {pdec}"
            sort = self._mk_sort(pdec[2])
            rv[castid(ActionBoundActionParamId,pdec[0])] = sort

        return rv

    def _mk_global_state_transform(self, statements:List[SExpr], a:Action) -> GlobalStateTransform:
        return GlobalStateTransform([self._mk_statement(x, a) for x in statements])

    def _mk_statement(self, statement:SExpr, parent_action:Action) -> GlobalStateTransformStatement:
        varname : str
        try:
            if statement[0] == APPLY_MACRO_LABEL:
                statement = self.handle_apply_macro(statement)

            if statement[0] == 'conjecture' or statement[0] == 'prove':
                self.assertOrSyntaxError( len(statement) == 2, statement, "GlobalStateTransformConjecture expression should have length 2")
                rhs = self._mk_term(statement[1], None, parent_action)
                return InCodeConjectureStatement(rhs)
            elif statement[0] == 'local':
                self.assertOrSyntaxError( len(statement) == 6, statement, 'Local var dec should have form (local name : type = term) or := instead of =')
                self.assertOrSyntaxError( statement[2] == ':' and (statement[4] == ":=" or statement[4] == "="), statement,
                                          'Local var dec should have form (local name : type = term)  or := instead of =')
                sort = self._mk_sort(statement[3])
                rhs = self._mk_term(statement[5], parent_action=parent_action)
                varname = castid(StateTransformLocalVarId,statement[1])
                lvd = StateTransformLocalVarDec(varname, rhs, sort)
                if varname in parent_action.local_vars:
                    self.syntaxError(statement, "Redeclaration of local variable")
                parent_action.local_vars[varname] = lvd
                return lvd
            elif statement[0] == 'if':
                test = self._mk_term(statement[1], None, parent_action, None)
                self.assertOrSyntaxError( isinstance(statement[2],SExpr) and isinstance(statement[4],SExpr), statement )
                true_branch = [
                    self._mk_statement(inner, parent_action) for inner in statement[2]
                ]
                self.assertOrSyntaxError(statement[3] == 'else', statement)
                false_branch = [
                    self._mk_statement(inner, parent_action) for inner in statement[4]
                ]
                return IfElse(test, true_branch, false_branch)

            else:
                self.assertOrSyntaxError( len(statement) == 3, statement, "As of 13 Aug 2017, every code block statement other than a conjecture or local var intro should be a triple: a := (or =), +=, -=, *= specifically. See\n" + str(statement))
                rhs = self._mk_term(statement[2], None, parent_action)
                varname = castid(GlobalVarId, statement[0])
                self.assertOrSyntaxError(varname in self.top.global_var_decs, statement, f"{varname} not recognized as a global state variable.")
                vardec = self.top.global_var_decs[varname]
                orig : GlobalStateTransformStatement
                reduced : GlobalStateTransformStatement
                if statement[1] == ':=' or statement[1] == "=":
                    return GlobalVarAssignStatement(vardec, rhs)
                else:
                    var = self.top.new_global_var_ref(varname)
                    if statement[1] == '+=':
                        orig = IncrementStatement(vardec, rhs)
                        reduced = GlobalVarAssignStatement(vardec, FnApp('+', [var, rhs]))
                    elif statement[1] == '-=':
                        orig = DecrementStatement(vardec, rhs)
                        reduced = GlobalVarAssignStatement(vardec, FnApp('-', [var, rhs]))
                    elif statement[1] == '*=':
                        orig = TimesEqualsStatement(vardec, rhs)
                        reduced = GlobalVarAssignStatement(vardec, FnApp('*', [var, rhs]))
                    else:
                        raise Exception
                    reduced.orig = orig
                    return reduced
        except Exception as e:
            logging.error(f"Problem with {statement}")
            raise e

    @staticmethod
    def mk_literal(x:str, parent_SExpr:Optional[SExpr] = None, prog:Optional[L4Contract] = None) -> Term:
        if isInt(x):
            return IntLit(int(x))
        if isFloat(x):
            return FloatLit(float(x))
        if x == 'false':
            return BoolLit(False)
        if x == 'true':
            return BoolLit(True)
        if x == 'never':
            return DeadlineLit(x)
        if x[-1].lower() in SUPPORTED_TIMEUNITS and isInt(x[:-1]):
            rv = SimpleTimeDeltaLit(int(x[:-1]), x[-1].lower())
            # print('STD', rv)
            return rv

        L4ContractConstructor.syntaxErrorX(parent_SExpr, f"Don't recognize name {x}")


    """
    parent_SExpr is used for debugging since s-expressions carry their original line number. not used for anything else. 
    """
    def _mk_term(self, x:Union[str, SExpr],
                 parent_section : Optional[Section] = None,
                 parent_action : Optional[Action] = None,
                 parent_action_rule : Optional[ActionRule] = None,
                 parent_SExpr : Optional[SExpr] = None ) -> Term:
        if isinstance(x,str):
            if x in EXEC_ENV_VARIABLES:
                return FnApp(x,[])

            if x in TIME_CONSTRAINT_KEYWORDS:
                return DeadlineLit(x)

            if x in self.top.global_var_decs:
                return GlobalVar(self.top.global_var_decs[cast(GlobalVarId,x)])

            # if parent_action and (x in parent_action.local_vars):
            #     return LocalVar(parent_action.local_vars[cast(LocalVarId,x)])

            if x in self.top.contract_params:
                return ContractParam(self.top.contract_params[cast(ContractParamId,x)])

            # print("parent_action_rule", parent_action_rule)
            # if x == 'order' and parent_action_rule:
            #     print("args", parent_action_rule.args)
            if parent_action_rule and parent_action_rule.args and x in parent_action_rule.args:
                assert parent_action_rule.args_name_to_ind is not None
                return RuleBoundActionParam(cast(RuleBoundActionParamId, x), parent_action_rule,
                                            parent_action_rule.args_name_to_ind[castid(RuleBoundActionParamId,x)])

            if parent_action and x in parent_action.param_sorts_by_name:
                assert parent_action.param_name_to_ind is not None
                return ActionBoundActionParam(cast(ActionBoundActionParamId, x), parent_action,
                                              parent_action.param_name_to_ind[castid(ActionBoundActionParamId,x)])

            if x in self.top.definitions:
                return self._mk_term(self.top.definitions[castid(DefinitionId, x)].body, parent_section, parent_action, parent_action_rule)

            if parent_action and x in parent_action.local_vars:
                return StateTransformLocalVar(parent_action.local_vars[castid(StateTransformLocalVarId,x)])

            return L4ContractConstructor.mk_literal(x, parent_SExpr, self.top)

        else: # SExpr
            if x[0] == APPLY_MACRO_LABEL:
                x = self.handle_apply_macro(x)

            pair = try_parse_as_fn_app(x)
            if pair:
                fnsymb_name = pair[0]
                # fnsymb_or_name = cast(Union[str,FnSymb], self.top.fnsymbs[fnsymb_name] if fnsymb_name in self.top.fnsymbs else fnsymb_name)
                self.top.fnsymb_names.add(fnsymb_name)
                args : List[Term]
                if fnsymb_name == 'cast':
                    args = [cast(Term,self._mk_sort_lit(pair[1][0]))] + [self._mk_term(arg, parent_section, parent_action, parent_action_rule, x) for arg in pair[1][1:]]
                else:
                    args = [ self._mk_term(arg, parent_section, parent_action, parent_action_rule, x) for arg in pair[1] ]
                return FnApp(
                    fnsymb_name,
                    args,
                    FileCoord(x.line, x.col)
                )
            else:
                if x in EXEC_ENV_VARIABLES:
                    self.syntaxError(x, f"You're using environment variable {x} like a 0-arg function symbol. Remove the brackets please.")
                else:
                    self.syntaxError(x, "Didn't recognize function symbol in: " + str(x))

                assert False # this is just to get mypy to not complain about missing return statement

    def _mk_time_constraint(self, expr:SExprOrStr, src_section:Optional[Section], src_action:Optional[Action], parent_action_rule:Optional[ActionRule]) -> Term:
        # if expr in TIME_CONSTRAINT_KEYWORDS:
        #     return self._mk_term(expr, src_section, src_action)
        # elif isinstance(expr,str):
        #     self.syntaxError(expr, f"Unrecognized token {expr} in time constraint keyword position.")
        if isinstance(expr,str):
            return self._mk_term(expr, src_section, src_action, parent_action_rule, None)
        else:
            self.assertOrSyntaxError( len(expr) > 1, expr)
            pair = try_parse_as_fn_app(expr)
            if pair and pair[0] in TIME_CONSTRAINT_PREDICATES:
                # return self._mk_term(expr, src_section, src_action, parent_action_rule, None)
                rv = FnApp(
                    pair[0],
                    [self._mk_term(arg, src_section, src_action, parent_action_rule, expr) for arg in pair[1]],
                    FileCoord(expr.line, expr.col)
                )
                # print("$ " + str(rv))
                return rv
            else:
                if src_section:
                    print("pair: ", pair)
                    self.syntaxError(expr, f"Unhandled time constraint predicate {expr} in section {src_section.section_id}")
                elif src_action:
                    self.syntaxError(expr, f"Unhandled time constraint predicate {expr} in action {src_action.action_id}")

        raise Exception("Must have time constraint. You can use `immediately` or `no_time_constraint` or `discretionary`")

    def _mk_future_action_rule(self, expr:SExpr, src_action:Action) -> PartyFutureActionRule:
        entrance_enabled_guard: Optional[Term] = None
        if expr[0] == 'if':
            entrance_enabled_guard = self._mk_term(expr[1], None, src_action)
            expr = expr[2]

        role_id = castid(RoleId, expr[0])
        deontic_keyword = castid(DeonticKeyword, expr[1])
        args: Optional[List[RuleBoundActionParamId]] = None
        if isinstance(expr[2],str):
            action_id = castid(ActionId,expr[2])
            args = []
        else:
            action_id = castid(ActionId, (expr[2][0]))

            args_part = expr[2][1:]
            if len(args_part) == 0:
                args = []
            elif args_part[0][0] == "?":
                assert all([args_part[i][0] == "?" for i in range(len(args_part))]), \
                    "Either all or none of the action argument positions in an action rule must be newly-bound variables prefixed with '?'."
                args = cast(List[RuleBoundActionParamId], args_part)

        if not is_derived_trigger_id(action_id):
            self.referenced_nonderived_action_ids.add(action_id)

        rv = PartyFutureActionRule(src_action.action_id, role_id, action_id, args, entrance_enabled_guard, deontic_keyword)
        if args is None:
            rv.fixed_args = [self._mk_term(arg, None, src_action, rv, args_part) for arg in args_part]

        rem = expr.tillEnd(3)
        assert len(rem) >= 1

        rv.time_constraint = self._mk_time_constraint(rem[0], None, src_action, rv)
        assert rv.time_constraint is not None, str(rem)

        for x in rem[1:]:
            if x[0] == "where":
                rv.where_clause = self._mk_term(x[1], None, src_action, rv)

        src_action.add_action_rule(rv)
        return rv


    def _mk_next_action_rule(self, expr:SExpr, src_section:Section, parent_action:Optional[Action]) -> NextActionRule:
        entrance_enabled_guard: Optional[Term] = None
        if expr[0] == 'if':
            entrance_enabled_guard = self._mk_term(expr[1], src_section)
            expr = expr[2]

        role_id : RoleId
        action_id : ActionId
        args : Optional[List[RuleBoundActionParamId]] = None
        rv : NextActionRule
        if len(expr) == 2:
            if isinstance(expr[0],str):
                action_id = castid(ActionId,expr[0])
                args = []
            else:
                action_id = castid(ActionId, (expr[0][0]))
                args_part = expr[0][1:]
                if len(args_part) == 0:
                    args = []
                elif args_part[0][0] == "?":
                    assert all([args_part[i][0] == "?" for i in range(len(args_part))]), \
                        "Either all or none of the action argument positions in an action rule must be newly-bound variables prefixed with '?'."
                    args = cast(List[RuleBoundActionParamId], args_part)

            if not is_derived_trigger_id(action_id):
                self.referenced_nonderived_action_ids.add(action_id)

            rv = EnvNextActionRule(src_section.section_id, action_id, args, entrance_enabled_guard)
            rem = expr.tillEnd(1)

        else:
            role_id = castid(RoleId, expr[0])
            deontic_keyword = castid(DeonticKeyword, expr[1])
            if isinstance(expr[2],str):
                action_id = castid(ActionId,expr[2])
                args = []
            else:
                action_id = castid(ActionId, (expr[2][0]))
                args_part = expr[2][1:]
                if len(args_part) == 0:
                    args = []
                elif args_part[0][0] == "?":
                    assert all([args_part[i][0] == "?" for i in range(len(args_part))]), \
                        "Either all or none of the action argument positions in an action rule must be newly-bound variables prefixed with '?'."
                    args = cast(List[RuleBoundActionParamId], args_part)

            if not is_derived_trigger_id(action_id):
                self.referenced_nonderived_action_ids.add(action_id)

            rv = PartyNextActionRule(src_section.section_id, role_id, action_id, args, entrance_enabled_guard, deontic_keyword)
            rem = expr.tillEnd(3)


        if args is None:
            rv.fixed_args = [self._mk_term(arg, src_section, parent_action, rv, args_part) for arg in args_part]
        assert len(rem) >= 1

        rv.time_constraint = self._mk_time_constraint(rem[0], src_section, parent_action, rv)
        assert rv.time_constraint is not None, str(rem)

        for x in rem[1:]:
            if x[0] == "where":
                rv.where_clause = self._mk_term(x[1], src_section, parent_action, rv)

        assert not rv.fixed_args or not rv.where_clause

        src_section.add_action_rule(rv)
        return rv

def try_parse_as_fn_app(x:SExpr)  -> Optional[Tuple[str, SExpr]]:
    return maybe_as_infix_fn_app(x) or maybe_as_prefix_fn_app(x) or maybe_as_postfix_fn_app(x)


def maybe_as_prefix_fn_app(se:SExpr) -> Optional[Tuple[str, SExpr]]:
    if isinstance(se[0],str):
        symb = se[0]
        if symb in PREFIX_FN_SYMBOLS:
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

