import logging
from typing import Tuple, cast, Optional, List

from model.util import streqci, list_split, caststr, isFloat, isInt
from model.statements import Term, ContractParamDec, Float, Bool, Int
from model.L4Contract import *
from parse_sexpr import prettySExprStr, SExpr, SExprOrStr, castse, STRING_LITERAL_MARKER, parse_file
from state_diagram_generation import contractToDotFile
from model.constants_and_defined_types import *

from model.ActionWithDestination import *




# def l4_model_from_sexpr(sexpr: List[SExpr])

class L4ContractConstructor:
    def __init__(self, filename:Optional[str] = None) -> None:
        self._top : L4Contract = L4Contract(filename or '')
        # includes ActionWithDestination
        self._referenced_section_ids: Set[SectionId] = set()
        self._referenced_action_ids: Set[ActionId] = set()

    def syntaxError(self, expr: SExpr, msg:str = None):
        raise SyntaxError((msg if msg else "") +
                          "\n" + str(expr) +
                          "\nline " + str(cast(SExpr, expr).line) +
                          "\n" + str(self._top.filename) )

    def assertOrSyntaxError(self, test:bool, expr:SExpr, msg:str = None):
        if not test:
            self.syntaxError(expr, msg)

    def top(self, l:List[SExpr]) -> L4Contract:
        x : SExpr
        for x in l:
            assert len(x) >= 2, "Problem top-level: " + str(x)
            rem = x.tillEnd(1)
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if   head( GLOBAL_VARS_SECTION_LABEL ):
                self._top.global_var_decs = self.global_vars(rem)

            elif head( CONTRACT_PARAMETERS_SECTION_LABEL ):
                # assert all(isinstance(expr[0],str) for expr in rem)
                self._top.contract_params = {caststr(expr[0]) : self.contract_param(expr) for expr in rem}

            elif head( CLAIMS_SECTION_LABEL ):
                self._top.claims = self.claims(rem)

            elif head( ROLES_SECTION_LABEL ):
                self._top.roles = self.actors(rem)

            elif head( PROSE_CONTRACT_SECTION_LABEL ):
                self._top.prose_contract = self.prose_contract(cast(List[List[str]],rem))

            elif head( FORMAL_CONTRACT_SECTION_LABEL ):
                self.formal_contract(rem)

            elif head( DOT_FILE_NAME_LABEL ):
                self._top.dot_file_name = caststr(x[1][1]) # the extra [1] is because its parse is of the form ['STRLIT', 'filename']
            elif head( IMG_FILE_NAME_LABEL ):
                self._top.img_file_name = caststr(x[1][1]) # the extra [1] is because its parse is of the form ['STRLIT', 'filename']

        if  self._referenced_section_ids != set(self._top.section_ids()).union([FULFILLED_SECTION_LABEL]):
            print(
            f"ISSUE\n:Set of referenced section ids ≠ set of declared section ids (plus '{FULFILLED_SECTION_LABEL}'):\n" +
            "Referenced           : " + str(sorted(self._referenced_section_ids)) + "\n" +
            f"Defined + '{FULFILLED_SECTION_LABEL}': " + str(sorted(set(self._top.section_ids()).union([FULFILLED_SECTION_LABEL])))
            )

        return self._top

    def contract_param(self, expr) -> ContractParamDec:
        self.assertOrSyntaxError( len(expr) == 5, expr, "Contract parameter dec should have form (name : type := term)" )
        return ContractParamDec(expr[0], expr[2], self.parse_term(expr[4]))

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
                self._top.sorts.add(sort)

                initval : Term = None
                if i+3 < len(dec) and dec[i+3] == ':=':
                    initval = self.parse_term(dec[i+4])
                print('initval', initval)
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

    def formal_contract(self, l:SExpr) -> None:
        self._top.contract_name = l[0][1] # [1] because STRLIT sexpr
        x: SExpr

        for x in l[1:]:
            assert len(x) >= 2
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if head(START_SECTION_LABEL):
                self.assertOrSyntaxError( len(x) == 2, l, "StartState declaration S-expression should have length 2")
                section_id = caststr(x[1])
                self._top.start_section = section_id
                self._referenced_section_ids.add(section_id)

            elif head(ACTION_LABEL):
                # (Action VerbAndParams transitionsto body)
                action_id : str
                action_data : SExpr
                action : Action
                assert len(x[2]) == 2
                destination_id = caststr(x[2][1])
                action_body = x.tillEnd(3)
                if isinstance(x[1], str):
                    # e.g. (Action SomethingHappens ...)
                    action_id = x[1]
                    action = self.action(action_id, destination_id, None, action_body)
                    self._top.actions_by_id[action_id] = action
                else:
                    # e.g. (Action (SomethingHappens param&sort1 param&sort2) ...)
                    action_id = caststr(x[1][0])
                    action_params = castse(x[1][1:])
                    action = self.action(action_id, destination_id, action_params, action_body)
                    self._top.actions_by_id[action_id] = action
                self._top.ordered_declarations.append(action)

            elif head(SECTION_LABEL):
                section_id = caststr(x[1])
                section_data = x.tillEnd(2)
                section = self.section(section_id, section_data)
                self._top.sections_by_id[section_id] = section
                self._top.ordered_declarations.append(section)

            elif head(COMPOUND_A) or head(COMPOUND_S):
                act_id: str
                dest_id: str
                id : str
                params : Optional[SExpr]
                body = x.tillEnd(2)
                if isinstance(x[1], str):
                    # e.g. (ActionWithDest SomethingHappens ...)
                    id = x[1]
                    params = None
                else:
                    # e.g. (Action (SomethingHappens param&sort1 param&sort2) ...)
                    id = caststr(x[1][0])
                    params = castse(x[1][1:])

                if head(COMPOUND_S):
                    dest_id = id
                    act_id = derived_trigger_id(dest_id)
                else:
                    act_id = id
                    dest_id = derived_destination_id(act_id)


                action = self.action(act_id, dest_id, params, body, True)
                self._top.actions_by_id[act_id] = action

                section = self.section(dest_id, body, True)
                self._top.sections_by_id[dest_id] = section

                pair = ActionWithDestination(action, section, head(COMPOUND_A))
                self._top.actionDestPair_by_id[id] = pair
                self._top.ordered_declarations.append(pair)

            else:
                self.syntaxError(l, f"Unrecognized head {x[0]}")

        # return FormalContract(caststr(l[0][1]), sections, start_section)


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
                if isinstance(x[1],SExpr) and isinstance(x[1][0],str) and (x[1][0] == 'guardsDisjointExhaustive' or x[1][0] == 'deadlinesDisjointExhaustive'):
                    x = x[1]
                    print("TODO: guardsDisjointExhaustive etc")
                connection_exprs = castse(x.tillEnd(1))
                for connection_expr in connection_exprs:
                    newconnection = self.connection(connection_expr, section)
                    self._top.connections.append(newconnection)

            # else:
            #     logging.warning("todo: handle " + x[0] + " in L4ContractConstructor.section")

            # elif head(EVENT_STATE_PROSE_REFS_LABEL):
            #     section.prose_refs = cast(List,x[1:]).copy()

        return section

    def action(self, action_id:ActionId, dest_id:SectionId, params:Optional[SExpr], rest:SExpr, is_compound = False) -> Action:
        a = Action(action_id, dest_id)
        a.is_compound = is_compound
        x: SExpr
        if isinstance(params, SExpr):
            a.params = self.action_params(cast(List[List[str]], params))
        for x in rest:
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if head(ACTION_DESCRIPTION_LABEL):
                a.action_description = caststr(x[1][1]) # extract from STRLIT expression

            elif head(CODE_BLOCK_LABEL):
                a.global_state_transform = self.global_state_transform(cast(List[SExpr],x[1:]), a)

            else:
                logging.warning("todo: handle " + x[0] + " in L4ContractConstructor.action")

            # elif head(EVENT_STATE_PROSE_REFS_LABEL):
            #     es.prose_refs = cast(List,x[1:]).copy()


        return a

    def action_params(self, parts:List[List[str]]) -> ParamsDec:
        pdec : List[str]
        for pdec in parts:
            assert len(pdec) == 3, f"Expected [<param name str>, ':', SORTstr] but got {pdec}"
            self._top.sorts.add(pdec[2])

        rv = {pdec[0]:pdec[2] for pdec in parts}
        # logging.info(str(rv))
        return rv

    def global_state_transform(self, statements:List[SExpr], a:Action) -> GlobalStateTransform:
        return GlobalStateTransform([self.global_state_transform_statement_dispatch(x,a) for x in statements])

    def global_state_transform_statement_dispatch(self, statement:SExpr, a:Action) -> CodeBlockStatement:
        try:
            if statement[0] == 'conjecture':
                self.assertOrSyntaxError( len(statement) == 2, statement, "GlobalStateTransformconjecture expression should have length 2")
                rhs = self.parse_term(statement[1])
                return InCodeConjectureStatement(cast(List,rhs))
            elif statement[0] == 'local':
                self.assertOrSyntaxError( len(statement) == 6, statement, 'Local var dec should have form (local name : type := term)')
                self.assertOrSyntaxError( statement[2] == ':' and (statement[4] == ":=" or statement[4] == "="), statement,
                                          'Local var dec should have form (local name : type := term)')
                sort = caststr(statement[3])
                rhs = self.parse_term(statement[5])
                varname = caststr(statement[1])
                lvd = LocalVarDec(varname, rhs, sort)
                if varname in a.local_vars:
                    self.syntaxError(statement, "Redeclaration of local variable")
                a.local_vars[varname] = lvd
                return lvd
            else:
                assert len(statement) == 3, "As of 13 Aug 2017, every code block statement other than a conjecture or local var intro should be a triple: a :=, +=, or -= specifically. See\n" + str(statement)
                rhs = self.parse_term(statement[2])
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

    def parse_term(self, x:Union[str,SExpr], parent_section:Optional[Section] = None, parent_action:Optional[Action] = None) -> Term:
        if isinstance(x,str):
            if x in self._top.global_var_decs:
                return GlobalVar(self._top.global_var_decs[x])
            if parent_action and (x in parent_action.local_vars):
                return LocalVar(x)
            if x in self._top.contract_params:
                return ContractParam(self._top.contract_params[x])
            if isInt(x):
                return Int(int(x))
            if isFloat(x):
                return Float(float(x))
            if x == 'false':
                return Bool(False)
            if x == 'true':
                return Bool(True)
            if x == 'never':
                return DeadlineLit(x)
            if x in DEADLINE_KEYWORDS:
                return DeadlineLit(x)

            logging.warning('Unrecognized atom: ' + x + '. Treating as deadline literal.')
            return DeadlineLit(x)
        elif isinstance(x,list) and len(x) == 2 and x[0] == STRING_LITERAL_MARKER:
            return StringLit(caststr(x[1]))
        else:
            pair = maybe_as_infix_fn_app(x) or maybe_as_prefix_fn_app(x) or maybe_as_postfix_fn_app(x)
            if not pair:
                logging.error("Didn't recognize function symbol in: " + str(x))
                self.syntaxError(x)
            return FnApp(pair[0], [self.parse_term(arg) for arg in pair[1]])

    def connection(self, expr:SExpr, src_section:Section) -> Connection:
        enabled_guard: Term = None
        if expr[0] == 'if':
            enabled_guard = self.parse_term(expr[1], src_section)
            expr = expr[2]

        role_id = caststr(expr[0])
        if role_id not in src_section.connections_by_role:
            src_section.connections_by_role[role_id] = list()
        deontic_keyword = caststr(expr[1])
        action_id: str = caststr(expr[2][0])
        args = castse(expr[2][1:])

        self._referenced_action_ids.add(action_id)

        rem = expr.tillEnd(3)
        deadline_clause : Term = None
        if len(rem) >= 1:
            assert len(rem) == 1
            if rem[0] in DEADLINE_KEYWORDS:
                assert len(rem) == 1
                deadline_clause = self.parse_term(rem[0], src_section)
            else:
                assert len(castse(rem[0])) > 1
                if rem[0][0] in DEADLINE_OPERATORS:
                    deadline_clause = self.parse_term(rem[0], src_section)

        c = Connection(src_id=src_section.section_id, role_id=role_id, action_id=action_id, args=args,
                        deontic_modality=deontic_keyword, deadline_clause=deadline_clause,
                        enabled_guard=enabled_guard)

        src_section.connections_by_role[role_id].append(c)
        return c

    # def nonactor_transition_clause(self,trans_spec, src_es_id:SectionId) -> Connection:
    #     try:
    #         dest_id: str = trans_spec[0]
    #         self._referenced_section_ids.add(dest_id)
    #         tc = Connection(src_es_id, dest_id, NONACTION_BLOCK_LABEL, None)
    #         tc.args = trans_spec[1]
    #         if len(trans_spec) > 2:
    #             if trans_spec[2] in DEADLINE_OPERATORS:
    #                 tc.conditions = trans_spec[2:4]
    #             else:
    #                 # assert trans_spec[2] in DEADLINE_KEYWORDS, trans_spec[2] + ' is not a deadline keyword'
    #                 tc.conditions = trans_spec[2:4]
    #             return tc
    #         return tc
    #     except Exception:
    #         logging.error(f"Problem processing {src_es_id} trans: " + str(trans_spec))
    #         return None

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

