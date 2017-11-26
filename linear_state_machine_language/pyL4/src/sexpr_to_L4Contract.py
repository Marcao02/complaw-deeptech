import logging
from typing import Tuple, cast, Callable

from correctness_checks import L4ContractConstructorInterface
from model.GlobalStateTransform import *
from model.BoundVar import LocalVar, GlobalVar, ContractParam, ConnectionDeclActionParam, ActionDeclActionParam
from model.GlobalStateTransformStatement import *
from model.L4Contract import *
from model.Literal import *
from model.SExpr import SExprOrStr
from model.Term import FnApp
from model.util import streqci, chcaststr, isFloat, isInt, todo_once, castid, chcast
from model.StringArgMacro import StringArgMacro
from parse_sexpr import castse, STRING_LITERAL_MARKER


class L4ContractConstructor(L4ContractConstructorInterface):
    def __init__(self, filename:Optional[str] = None) -> None:
        self.top : L4Contract = L4Contract(filename or '')
        self.referenced_nonderived_section_ids: Set[SectionId] = set()
        self.referenced_nonderived_action_ids: Set[ActionId] = set()
        self.after_model_build_requirements : List[Callable[[],bool]]

    def syntaxError(self, expr: SExprOrStr, msg:Optional[str] = None):
        if isinstance(expr,SExpr):
            raise SyntaxError((msg if msg else "") +
                              "\n" + str(expr) +
                              "\nline " + str(cast(SExpr, expr).line) +
                              "\n" + str(self.top.filename))
        else:
            raise SyntaxError((msg if msg else "") +
                              "\n" + str(self.top.filename))

    def assertOrSyntaxError(self, test:bool, expr:SExpr, msg:Optional[str] = None):
        if not test:
            self.syntaxError(expr, msg)

    def l4contract(self, l:List[SExpr]) -> L4Contract:
        x : SExpr
        for x in l:
            #assert len(x) >= 2, "Problem top-level: " + str(x)
            rem = x.tillEnd(1)
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if   head( STR_ARG_MACRO_DEC_LABEL):
                macroname = chcaststr(x[1])
                macroparam = chcaststr(x[2])
                macrobody = chcast(SExpr, x[3])
                self.top.str_arg_macros[ macroname ] = StringArgMacro(macroparam, macrobody)

            elif   head(GLOBAL_VARS_AREA_LABEL):
                self.top.global_var_decs = self.global_vars(rem)

            elif head(CONTRACT_PARAMETERS_AREA_LABEL):
                # assert all(isinstance(expr[0],str) for expr in rem)
                self.top.contract_params = {castid(ContractParamId,expr[0]) : self.contract_param(expr) for expr in rem}

            elif head(TOPLEVEL_CLAIMS_AREA_LABEL):
                self.top.claims = self.claims(rem)

            elif head(ROLES_DEC_LABEL):
                self.top.roles = self.actors(rem)

            elif head(PROSE_CONTRACT_AREA_LABEL):
                self.top.prose_contract = self.prose_contract(cast(List[List[str]], rem))

            elif head(FORMAL_CONTRACT_AREA_LABEL):
                self.construct_main_part(rem)

            elif head( TIME_UNIT_DEC_LABEL ):
                self.top.time_unit = chcaststr(x[1])

            elif head( DEFINITIONS_AREA ):
                self.top.definitions = self.definitions(rem)

            elif head( DOT_FILE_NAME_LABEL ):
                self.top.dot_file_name = chcaststr(x[1][1]) # the extra [1] is because its parse is of the form ['STRLIT', 'filename']
            elif head( IMG_FILE_NAME_LABEL ):
                self.top.img_file_name = chcaststr(x[1][1]) # the extra [1] is because its parse is of the form ['STRLIT', 'filename']

            else:
                raise Exception("Unsupported: ", x[0])

        return self.top

    def contract_param(self, expr) -> ContractParamDec:
        self.assertOrSyntaxError( len(expr) == 5, expr, "Contract parameter dec should have form (name : type := term)" )
        return ContractParamDec(expr[0], expr[2], self.term(expr[4]))

    def global_vars(self, l:SExpr) -> Dict[GlobalVarId,GlobalVarDec]:
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
                sort = cast(SortId, chcaststr(dec[i + 2]))
                self.top.sorts.add(sort)

                initval : Optional[Term] = None
                if i+3 < len(dec) and (dec[i+3] == ':=' or dec[i+3] == '='):
                    initval = self.term(dec[i + 4])
                # print("sort: ", str(sort))
                # print("initval: ", str(initval))
                rv[name] = GlobalVarDec(name, sort, initval, modifiers)
            except Exception as e:
                logging.error("Problem processing " + str(dec))
                raise e

        # logging.info(str(rv))
        return rv

    def claims(self, l:SExpr) -> List[ContractClaim]:
        rv = [ContractClaim(x) for x in l]
        # logging.info(str(rv))
        return rv

    def actors(self, s:SExpr) -> List[RoleId]:
        # logging.info(str(l))
        self.assertOrSyntaxError(all(isinstance(x, str) for x in s), s, "Actors declaration S-expression should have the form (Actors Alice Bob)")
        return cast(List[RoleId],s.lst.copy())

    def definitions(self, s:SExpr) -> Dict[DefinitionId, Definition]:
        self.assertOrSyntaxError(all(len(x) == 3 for x in s), s,
                                 "Definition declaration S-expressions should have the form (id = SExpr)")
        return {x[0] : Definition(x[0],x[2]) for x in s}

    def prose_contract(self, l: List[List[str]]) -> ProseContract:
        rv = {castid(ProseClauseId,x[0]): x[1] for x in l}
        # logging.info(str(rv))
        return rv

    def handle_apply_macro(self, x:SExpr) -> SExpr:
        macroname = chcaststr(x[1])
        macro = self.top.str_arg_macros[macroname]
        arg = chcaststr(x[2])
        return macro.subst(arg)


    def construct_main_part(self, l:SExpr) -> None:
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
                    action = self.action(action_id, None, action_body)
                    self.top.actions_by_id[action_id] = action
                else:
                    # e.g. (Action (SomethingHappens param&sort1 param&sort2) ...)
                    action_id = cast(ActionId, chcaststr(x[1][0]))
                    action_params = cast(List[List[str]], x[1][1:])
                    action = self.action(action_id, action_params, action_body)
                    self.top.actions_by_id[action_id] = action
                self.top.ordered_declarations.append(action)

            elif head(SECTION_LABEL):
                section_id = cast(SectionId, chcaststr(x[1]))
                section_data = x.tillEnd(2)
                section = self.section(section_id, section_data)
                self.top.sections_by_id[section_id] = section
                self.top.ordered_declarations.append(section)

            else:
                self.syntaxError(l, f"Unrecognized head {x[0]}")


    def section(self, section_id:SectionId, rest:SExpr) -> Section:
        section = Section(section_id)
        x: SExpr
        for x in rest:
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if head(SECTION_PRECONDITION_LABEL):
                section.preconditions.append( self.term(x[1], section) )

            elif head(SECTION_DESCRIPTION_LABEL):
                section.section_description = chcaststr(x[1][1]) # extract from STRLIT expression

            elif head(OUT_CONNECTIONS_LABEL):
                if isinstance(x[1],SExpr) and isinstance(x[1][0],str) and (x[1][0] == 'guardsDisjointExhaustive' or x[1][0] == 'deadlinesPartitionFuture'):
                    x = x[1]
                    todo_once("guardsDisjointExhaustive etc in section()")
                connection_exprs = castse(x.tillEnd(1))
                for connection_expr in connection_exprs:
                    if connection_expr[0] == APPLY_MACRO_LABEL:
                        connection_expr = self.handle_apply_macro(connection_expr)

                    newconnection = self.connection(connection_expr, section)
                    self.top.connections.append(newconnection)

            elif head(PROSE_REFS_LABEL):
                section.prose_refs = cast(List,x[1:]).copy()

            elif 'visits' in x or 'traversals' in x:
                section.visit_bounds = x # self.term(x, None, section)

            else:
                self.syntaxError(x, f"Unsupported declaration type {x[0]} in section {section_id}")
                todo_once(f"Handle {x[0]} in section dec")

        return section

    def action(self, action_id:ActionId, params_sexpr:Optional[List[List[str]]], rest:SExpr) -> Action:
        a = Action(action_id)
        dest_section_id = None
        x: SExpr
        if params_sexpr is not None: #isinstance(params_sexpr, SExpr):
            a.params = self.action_params(params_sexpr)
        for x in rest:
            def head(constant:str) -> bool:
                nonlocal x
                if len(x) == 0:
                    print("problem", x)
                return streqci(x[0], constant)

            if head(ACTION_PRECONDITION_LABEL):
                a.preconditions.append(self.term(x[1], None, a))

            elif head(ACTION_POSTCONDITION_LABEL):
                a.postconditions.append(self.term(x[1], None, a))

            elif head(ACTION_DESCRIPTION_LABEL):
                a.action_description = chcaststr(x[1][1]) # extract from STRLIT expression

            elif head(CODE_BLOCK_LABEL):
                a.global_state_transform= self.global_state_transform(cast(List[SExpr],x.tillEnd(1).lst), a)

            elif head(PROSE_REFS_LABEL):
                a.prose_refs  = cast(List[str], x.tillEnd(1).lst)

            elif head(TRANSITIONS_TO_LABEL):
                dest_section_id = x[1]
                if not is_derived_destination_id(dest_section_id):
                    self.referenced_nonderived_section_ids.add(dest_section_id)

            elif 'traversals' in x or 'visits' in x:
                a.traversal_bounds = x # self.term(x, None, a)

            elif head(ALLOWED_SUBJECTS_DEC_LABEL):
                a.allowed_subjects = x.tillEnd(1)

            elif head(FOLLOWING_SECTION_DEC_LABEL):
                anon_sect_id : str
                if is_derived_trigger_id(action_id):
                    anon_sect_id = derived_trigger_id_to_section_id(action_id)
                else:
                    anon_sect_id = derived_destination_id(action_id)
                a.following_anon_section  = self.section(anon_sect_id, x.tillEnd(1))
                a.following_anon_section.parent_action_id = action_id
                self.top.sections_by_id[a.following_anon_section.section_id] = a.following_anon_section

            else:
                todo_once(f"Handle {x[0]} in action dec")

        if dest_section_id:
            a.dest_section_id = dest_section_id
        else:
            if is_derived_trigger_id(a.action_id):
                a.dest_section_id = derived_trigger_id_to_section_id(a.action_id)
                self.referenced_nonderived_section_ids.add(a.dest_section_id)
            else:
                a.dest_section_id = derived_destination_id(a.action_id)

        return a

    def action_params(self, parts:List[List[str]]) -> ParamsDec:
        pdec : List[str]
        for pdec in parts:
            assert len(pdec) == 3, f"Expected [<param name str>, ':', SORTstr] but got {pdec}"
            self.top.sorts.add(castid(SortId,pdec[2]))

        rv = {castid(ActionParamId,pdec[0]) : castid(SortId,pdec[2]) for pdec in parts}
        # logging.info(str(rv))
        return rv

    def global_state_transform(self, statements:List[SExpr], a:Action) -> GlobalStateTransform:
        return GlobalStateTransform([self.global_state_transform_statement_dispatch(x,a) for x in statements])

    def global_state_transform_statement_dispatch(self, statement:SExpr, parent_action:Action) -> GlobalStateTransformStatement:
        try:
            if statement[0] == APPLY_MACRO_LABEL:
                statement = self.handle_apply_macro(statement)

            if statement[0] == 'conjecture':
                self.assertOrSyntaxError( len(statement) == 2, statement, "GlobalStateTransformconjecture expression should have length 2")
                rhs = self.term(statement[1], None, parent_action)
                return InCodeConjectureStatement(rhs)
            elif statement[0] == 'local':
                self.assertOrSyntaxError( len(statement) == 6, statement, 'Local var dec should have form (local name : type := term)')
                self.assertOrSyntaxError( statement[2] == ':' and (statement[4] == ":=" or statement[4] == "="), statement,
                                          'Local var dec should have form (local name : type := term)')
                sort = chcaststr(statement[3])
                rhs = self.term(statement[5])
                varname = castid(LocalVarId,statement[1])
                lvd = LocalVarDec(varname, rhs, sort)
                if varname in parent_action.local_vars:
                    self.syntaxError(statement, "Redeclaration of local variable")
                parent_action.local_vars[varname] = lvd
                return lvd
            elif statement[0] == 'if':
                test = self.term(statement[1], None, parent_action, None)
                self.assertOrSyntaxError( isinstance(statement[2],SExpr) and isinstance(statement[4],SExpr), statement )
                true_branch = [
                    self.global_state_transform_statement_dispatch(inner, parent_action) for inner in statement[2]
                ]
                self.assertOrSyntaxError(statement[3] == 'else', statement)
                false_branch = [
                    self.global_state_transform_statement_dispatch(inner, parent_action) for inner in statement[4]
                ]
                return IfElse(test, true_branch, false_branch)

            else:
                self.assertOrSyntaxError( len(statement) == 3, statement, "As of 13 Aug 2017, every code block statement other than a conjecture or local var intro should be a triple: a :=, +=, or -= specifically. See\n" + str(statement))
                rhs = self.term(statement[2], None, parent_action)
                varname2 = castid(LocalOrGlobalVarId, statement[0])
                if statement[1] == ':=' or statement[1] == "=":
                    return VarAssignStatement(varname2, rhs)
                elif statement[1] == '+=':
                    return IncrementStatement(varname2, rhs)
                elif statement[1] == '-=':
                    return DecrementStatement(varname2, rhs)
                elif statement[1] == '*=':
                    return TimesEqualsStatement(varname2, rhs)
                else:
                    raise Exception
                return None # not reachable
        except Exception as e:
            logging.error(f"Problem with {statement}")
            raise e

    @staticmethod
    def literal(x:str) -> Term:
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
        raise SyntaxError(f'Unrecognized atom: {x}')

    def term(self, x:Union[str, SExpr],
             parent_section : Optional[Section] = None, parent_action : Optional[Action] = None,
             parent_connection : Optional[Connection] = None) -> Term:
        if isinstance(x,str):
            if x in self.top.global_var_decs:
                return GlobalVar(self.top.global_var_decs[cast(GlobalVarId,x)])
            if parent_action and (x in parent_action.local_vars):
                return LocalVar(parent_action.local_vars[cast(LocalVarId,x)])
            if x in self.top.contract_params:
                return ContractParam(self.top.contract_params[cast(ContractParamId,x)])
            if x in DEADLINE_KEYWORDS:
                return DeadlineLit(x)
            if parent_connection and parent_connection.args and x in parent_connection.args:
                return ConnectionDeclActionParam(cast(ConnectionActionParamId, x), parent_connection )
            if parent_action and parent_action.params and x in parent_action.params:
                return ActionDeclActionParam(cast(ActionParamId, x), parent_action)
            if x in self.top.definitions:
                return self.term(self.top.definitions[castid(DefinitionId,x)].body, parent_section, parent_action, parent_connection)
            return L4ContractConstructor.literal(x)
            # if isInt(x):
            #     return IntLit(int(x))
            # if isFloat(x):
            #     return FloatLit(float(x))
            # if x == 'false':
            #     return BoolLit(False)
            # if x == 'true':
            #     return BoolLit(True)
            # if x == 'never':
            #     return DeadlineLit(x)

        elif isinstance(x,list) and len(x) == 2 and x[0] == STRING_LITERAL_MARKER:
            raise Exception("can this still happen??")
            return StringLit(chcaststr(x[1]))

        else: # SExpr
            if x[0] == APPLY_MACRO_LABEL:
                x = self.handle_apply_macro(x)

            pair = maybe_as_infix_fn_app(x) or maybe_as_prefix_fn_app(x) or maybe_as_postfix_fn_app(x)
            if pair:
                return FnApp(
                    pair[0],
                    [self.term(arg, parent_section, parent_action, parent_connection) for arg in pair[1]]
                )
            else:
                self.syntaxError(x, "Didn't recognize function symbol in: " + str(x))
                raise SyntaxError() # this is just to get mypy to not complain about missing return statement


    def deadline_clause(self, expr:SExpr, src_section:Section) -> Term:
        if expr in DEADLINE_KEYWORDS:
            return self.term(expr, src_section)
        else:
            assert len(expr) > 1
            if expr[0] in DEADLINE_PREDICATES:
                return self.term(expr, src_section)
            else:
                self.syntaxError(expr, "Unhandled deadline predicate in L4ContractConstructor.deadline_clause(): " + str(expr))
        raise Exception("Must have deadline clause. You can use `immediately` or `nodeadline` or `discretionary`")

    def connection(self, expr:SExpr, src_section:Section) -> Connection:
        entrance_enabled_guard: Optional[Term] = None
        if expr[0] == 'if':
            entrance_enabled_guard = self.term(expr[1], src_section)
            expr = expr[2]

        role_id : RoleId
        action_id : ActionId
        args : Optional[List[ConnectionActionParamId]]
        rv : Connection
        if len(expr) == 2:
            action_id = castid(ActionId, expr[0])
            role_id = ENV_ROLE
            if not is_derived_trigger_id(action_id):
                self.referenced_nonderived_action_ids.add(action_id)
            args = None
            rem = expr.tillEnd(1)
            rv = ConnectionToEnvAction(src_section.section_id, action_id, args, entrance_enabled_guard)
        else:
            role_id = castid(RoleId, expr[0])
            deontic_keyword = castid(DeonticModality, expr[1])
            if isinstance(expr[2],str):
                action_id = castid(ActionId,expr[2])
                args = None
            else:
                action_id = castid(ActionId, (expr[2][0]))
                args = cast(List[ConnectionActionParamId], expr[2][1:])

            if not is_derived_trigger_id(action_id):
                self.referenced_nonderived_action_ids.add(action_id)

            rv = ConnectionToAction(src_section.section_id, role_id, action_id, args, entrance_enabled_guard, deontic_keyword)
            rem = expr.tillEnd(3)

        if role_id not in src_section.connections_by_role:
            src_section.connections_by_role[role_id] = list()
        assert len(rem) >= 1

        rv.deadline_clause = self.deadline_clause(rem[0], src_section)
        assert rv.deadline_clause is not None, str(rem)

        for x in rem[1:]:
            if x[0] == "where":
                rv.where_clause = self.term(x[1], src_section, None, rv)

        src_section.connections_by_role[role_id].append(rv)
        return rv

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
            return symb, se.withDropped(1)
    return None

def maybe_as_postfix_fn_app(se:SExpr) -> Optional[Tuple[str, SExpr]]:
    if isinstance(se[-1],str):
        symb = se[-1]
        if symb in POSTFIX_FN_SYMBOLS:
            return symb, se.fromStartToExclusive(len(se) - 1)
    return None

