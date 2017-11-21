import logging
from typing import Tuple, cast, Callable

from correctness_checks import L4ContractConstructorInterface
from model.GlobalStateTransform import *
from model.BoundVar import LocalVar, GlobalVar, ContractParam, ConnectionDeclActionParam, ActionDeclActionParam
from model.GlobalStateTransformStatement import *
from model.L4Contract import *
from model.Literal import *
from model.Term import FnApp
from model.util import streqci, caststr, isFloat, isInt, todo_once
from parse_sexpr import castse, STRING_LITERAL_MARKER


class L4ContractConstructor(L4ContractConstructorInterface):
    def __init__(self, filename:Optional[str] = None) -> None:
        self.top : L4Contract = L4Contract(filename or '')
        self.referenced_nonderived_section_ids: Set[SectionId] = set()
        self.referenced_nonderived_action_ids: Set[ActionId] = set()
        self.after_model_build_requirements : List[Callable[[],bool]]

    def syntaxError(self, expr: SExpr, msg:Optional[str] = None):
        raise SyntaxError((msg if msg else "") +
                          "\n" + str(expr) +
                          "\nline " + str(cast(SExpr, expr).line) +
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

            if   head( GLOBAL_VARS_SECTION_LABEL ):
                self.top.global_var_decs = self.global_vars(rem)

            elif head( CONTRACT_PARAMETERS_SECTION_LABEL ):
                # assert all(isinstance(expr[0],str) for expr in rem)
                self.top.contract_params = {caststr(expr[0]) : self.contract_param(expr) for expr in rem}

            elif head( CLAIMS_SECTION_LABEL ):
                self.top.claims = self.claims(rem)

            elif head( ROLES_SECTION_LABEL ):
                self.top.roles = self.actors(rem)

            elif head( PROSE_CONTRACT_SECTION_LABEL ):
                self.top.prose_contract = self.prose_contract(cast(List[List[str]], rem))

            elif head( FORMAL_CONTRACT_SECTION_LABEL ):
                self.construct_main_part(rem)

            elif head( DOT_FILE_NAME_LABEL ):
                self.top.dot_file_name = caststr(x[1][1]) # the extra [1] is because its parse is of the form ['STRLIT', 'filename']
            elif head( IMG_FILE_NAME_LABEL ):
                self.top.img_file_name = caststr(x[1][1]) # the extra [1] is because its parse is of the form ['STRLIT', 'filename']

        return self.top

    def contract_param(self, expr) -> ContractParamDec:
        self.assertOrSyntaxError( len(expr) == 5, expr, "Contract parameter dec should have form (name : type := term)" )
        return ContractParamDec(expr[0], expr[2], self.term(expr[4]))

    def global_vars(self, l:SExpr) -> Dict[str,GlobalVarDec]:
        rv = dict()
        for dec in l:
            try:
                i = 0
                modifiers : List[str] = []
                while True:
                    if dec[i] in VARIABLE_MODIFIERS:
                        modifiers.append(caststr(dec[i]))
                        i += 1
                    else:
                        break
                name = caststr(dec[i])
                sort = caststr(dec[i+2])
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

    def actors(self, s:SExpr) -> List[str]:
        # logging.info(str(l))
        self.assertOrSyntaxError(all(isinstance(x, str) for x in s), s, "Actors declaration S-expression should have the form (Actors Alice Bob)")
        return cast(List[str],s.lst.copy())

    def prose_contract(self, l: List[List[str]]) -> ProseContract:
        rv = {x[0]: x[1] for x in l}
        # logging.info(str(rv))
        return rv

    def construct_main_part(self, l:SExpr) -> None:
        self.top.contract_name = l[0][1] # [1] because STRLIT sexpr
        x: SExpr

        for x in l[1:]:
            assert len(x) >= 2
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if head(START_SECTION_LABEL):
                self.assertOrSyntaxError( len(x) == 2, l, "StartState declaration S-expression should have length 2")
                section_id = caststr(x[1])
                self.top.start_section = section_id
                if not is_derived_destination_id(section_id):
                    self.referenced_nonderived_section_ids.add(section_id)

            elif head(ACTION_LABEL):
                action_id : str
                action : Action
                action_body = x.tillEnd(2)
                if isinstance(x[1], str):
                    # e.g. (Action SomethingHappens ...)
                    action_id = x[1]
                    action = self.action(action_id, None, action_body)
                    self.top.actions_by_id[action_id] = action
                else:
                    # e.g. (Action (SomethingHappens param&sort1 param&sort2) ...)
                    action_id = caststr(x[1][0])
                    action_params = cast(List[List[str]], x[1][1:])
                    action = self.action(action_id, action_params, action_body)
                    self.top.actions_by_id[action_id] = action
                self.top.ordered_declarations.append(action)

            elif head(SECTION_LABEL):
                section_id = caststr(x[1])
                section_data = x.tillEnd(2)
                section = self.section(section_id, section_data)
                self.top.sections_by_id[section_id] = section
                self.top.ordered_declarations.append(section)

            else:
                self.syntaxError(l, f"Unrecognized head {x[0]}")


    def section(self, section_id:SectionId, rest:SExpr, is_compound = False) -> Section:
        section = Section(section_id)
        section.is_compound = is_compound
        x: SExpr
        for x in rest:
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if head(SECTION_DESCRIPTION_LABEL):
                section.section_description = caststr(x[1][1]) # extract from STRLIT expression

            elif head(OUT_CONNECTIONS_LABEL):
                if isinstance(x[1],SExpr) and isinstance(x[1][0],str) and (x[1][0] == 'guardsDisjointExhaustive' or x[1][0] == 'deadlinesPartitionFuture'):
                    x = x[1]
                    todo_once("guardsDisjointExhaustive etc in section()")
                connection_exprs = castse(x.tillEnd(1))
                for connection_expr in connection_exprs:
                    newconnection = self.connection(connection_expr, section)
                    self.top.connections.append(newconnection)

            elif head(PROSE_REFS_LABEL):
                section.prose_refs = cast(List,x[1:]).copy()

            elif 'visits' in x or 'traversals' in x:
                section.visit_bounds = x # self.term(x, None, section)

            else:
                todo_once(f"Handle {x[0]} in section dec")

        self.top.max_section_id_len = max(len(section_id), self.top.max_section_id_len)
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
                return streqci(x[0], constant)

            if head(ACTION_DESCRIPTION_LABEL):
                a.action_description = caststr(x[1][1]) # extract from STRLIT expression

            elif head(CODE_BLOCK_LABEL):
                a.global_state_transform= self.global_state_transform(cast(List[SExpr],x[1:]), a)

            elif head(PROSE_REFS_LABEL):
                a.prose_refs  = x[1:].lst

            elif head(TRANSITIONS_TO_LABEL):
                dest_section_id = x[1]
                if not is_derived_destination_id(dest_section_id):
                    self.referenced_nonderived_section_ids.add(dest_section_id)

            elif 'traversals' in x or 'visits' in x:
                a.traversal_bounds = x # self.term(x, None, a)

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


        self.top.max_action_id_len = max(len(action_id), self.top.max_action_id_len)

        return a

    def action_params(self, parts:List[List[str]]) -> ParamsDec:
        pdec : List[str]
        for pdec in parts:
            assert len(pdec) == 3, f"Expected [<param name str>, ':', SORTstr] but got {pdec}"
            self.top.sorts.add(pdec[2])

        rv = {pdec[0]:pdec[2] for pdec in parts}
        # logging.info(str(rv))
        return rv

    def global_state_transform(self, statements:List[SExpr], a:Action) -> GlobalStateTransform:
        return GlobalStateTransform([self.global_state_transform_statement_dispatch(x,a) for x in statements])

    def global_state_transform_statement_dispatch(self, statement:SExpr, parent_action:Action) -> GlobalStateTransformStatement:
        try:
            if statement[0] == 'conjecture':
                self.assertOrSyntaxError( len(statement) == 2, statement, "GlobalStateTransformconjecture expression should have length 2")
                rhs = self.term(statement[1], None, parent_action)
                return InCodeConjectureStatement(rhs)
            elif statement[0] == 'local':
                self.assertOrSyntaxError( len(statement) == 6, statement, 'Local var dec should have form (local name : type := term)')
                self.assertOrSyntaxError( statement[2] == ':' and (statement[4] == ":=" or statement[4] == "="), statement,
                                          'Local var dec should have form (local name : type := term)')
                sort = caststr(statement[3])
                rhs = self.term(statement[5])
                varname = caststr(statement[1])
                lvd = LocalVarDec(varname, rhs, sort)
                if varname in parent_action.local_vars:
                    self.syntaxError(statement, "Redeclaration of local variable")
                parent_action.local_vars[varname] = lvd
                return lvd
            else:
                assert len(statement) == 3, "As of 13 Aug 2017, every code block statement other than a conjecture or local var intro should be a triple: a :=, +=, or -= specifically. See\n" + str(statement)
                rhs = self.term(statement[2], None, parent_action)
                varname = caststr(statement[0])
                if statement[1] == ':=' or statement[1] == "=":
                    return VarAssignStatement(varname, rhs)
                elif statement[1] == '+=':
                    return IncrementStatement(varname, rhs)
                elif statement[1] == '-=':
                    return DecrementStatement(varname, rhs)
                elif statement[1] == '*=':
                    return TimesEqualsStatement(varname, rhs)
                else:
                    raise Exception
                return None # not reachable
        except Exception as e:
            logging.error(f"Problem with {statement}")
            raise e

    def term(self, x:Union[str, SExpr],
             parent_section : Optional[Section] = None, parent_action : Optional[Action] = None,
             parent_connection : Optional[Connection] = None) -> Term:
        if isinstance(x,str):
            if x in self.top.global_var_decs:
                return GlobalVar(self.top.global_var_decs[x])
            if parent_action and (x in parent_action.local_vars):
                return LocalVar(parent_action.local_vars[x])
            if x in self.top.contract_params:
                return ContractParam(self.top.contract_params[x])
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
            if x in DEADLINE_KEYWORDS:
                return DeadlineLit(x)
            if parent_connection and parent_connection.args and x in parent_connection.args:
                return ConnectionDeclActionParam(x, parent_connection )
            if parent_action and parent_action.params and x in parent_action.params:
                return ActionDeclActionParam(x, parent_action)

            logging.warning('Unrecognized atom: ' + x + '. Treating as deadline literal.')
            return DeadlineLit(x)
        elif isinstance(x,list) and len(x) == 2 and x[0] == STRING_LITERAL_MARKER:
            return StringLit(caststr(x[1]))
        else:
            pair = maybe_as_infix_fn_app(x) or maybe_as_prefix_fn_app(x) or maybe_as_postfix_fn_app(x)
            if pair:
                return FnApp(
                    pair[0],
                    [self.term(arg, parent_section, parent_action, parent_connection) for arg in pair[1]]
                )
            else:
                self.syntaxError(x)
                raise SyntaxError("Didn't recognize function symbol in: " + str(x))


    def deadline_clause(self, expr:SExpr, src_section:Section) -> Term:
        if expr in DEADLINE_KEYWORDS:
            return self.term(expr, src_section)
        else:
            assert len(expr) > 1
            if expr[0] in DEADLINE_PREDICATES:
                return self.term(expr, src_section)
            else:
                logging.error("Unhandled case in L4ContractConstructor.deadline_clause(): " + str(expr))
        raise Exception("Must have deadline clause. You can use `immediately` or `nodeadline` or `discretionary`")

    def connection(self, expr:SExpr, src_section:Section) -> Connection:
        enabled_guard: Optional[Term] = None
        if expr[0] == 'if':
            enabled_guard = self.term(expr[1], src_section)
            expr = expr[2]

        role_id : str
        action_id : str
        args : Optional[List[str]]
        rv : Connection
        if len(expr) == 2:
            action_id = expr[0]
            role_id = ENV_ROLE
            if not is_derived_trigger_id(action_id):
                self.referenced_nonderived_action_ids.add(action_id)
            args = None
            rem = expr.tillEnd(1)
            rv = ConnectionToEnvAction(src_section.section_id, action_id, args, enabled_guard)
        else:
            role_id = caststr(expr[0])
            deontic_keyword = caststr(expr[1])
            action_id = caststr(expr[2][0])
            if not is_derived_trigger_id(action_id):
                self.referenced_nonderived_action_ids.add(action_id)
            args = cast(List[str], expr[2][1:])
            rv = ConnectionToAction(src_section.section_id, role_id, action_id, args, enabled_guard, deontic_keyword)
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

