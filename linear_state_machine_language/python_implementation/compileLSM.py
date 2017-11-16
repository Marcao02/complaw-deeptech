import logging

from LSMTop import *
from parse_sexpr import prettySExprStr, SExpr, SExprOrStr, castse, STRING_LITERAL_MARKER, parse_file
from state_diagram_generation import contractToDotFile
from util import streqci, list_split, caststr, isFloat, isInt
from typing import Tuple, cast, Optional, List
from LSMStatements import Term, ContractParamDec, Float, Bool, Int

class Assemble:
    def __init__(self, filename:str) -> None:
        self._top : LSMTop = LSMTop(filename)
        self._referenced_event_stateids: Set[EventStateId] = set()

    def syntaxError(self, expr: SExpr, msg:str = None):
        raise SyntaxError((msg if msg else "") +
                          "\n" + str(expr) +
                          "\nline " + str(cast(SExpr, expr).line) +
                          "\n" + str(self._top.filename) )

    def assertOrSyntaxError(self, test:bool, expr:SExpr, msg:str = None):
        if not test:
            self.syntaxError(expr, msg)

    def top(self, l:List[SExpr]):
        x : SExpr
        for x in l:
            assert len(x) >= 2, "Problem top-level: " + str(x)
            rem = x.tillEnd(1)
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if   head( GLOBAL_VARS_SECTION_LABEL ):
                assert isinstance(x,SExpr), type(x)
                self._top.global_var_decs = self.global_vars(rem)
            elif head( CONTRACT_PARAMETERS_SECTION_LABEL ):
                # assert all(isinstance(expr[0],str) for expr in rem)
                self._top.contract_params = {caststr(expr[0]) : self.contract_param(expr) for expr in rem}
            elif head( CLAIMS_SECTION_LABEL ):
                self._top.claims = self.claims(rem)
            elif head( ACTORS_SECTION_LABEL ):
                self._top.actors = self.actors(rem)
            elif head( PROSE_CONTRACT_SECTION_LABEL ):
                self._top.prose_contract = self.prose_contract(cast(List[List[str]],rem))
            elif head( FORMAL_CONTRACT_SECTION_LABEL ):
                self._top.formal_contract = self.formal_contract(rem)
            elif head( DOT_FILE_NAME_LABEL ):
                self._top.dot_file_name = caststr(x[1][1]) # the extra [1] is because its parse is of the form ['STRLIT', 'filename']
            elif head( IMG_FILE_NAME_LABEL ):
                self._top.img_file_name = caststr(x[1][1]) # the extra [1] is because its parse is of the form ['STRLIT', 'filename']

        if  self._referenced_event_stateids != set(self._top.formal_contract.estates.keys()).union([FULFILLED_EVENT_STATE_LABEL]):
            print(
            f"ISSUE\n:Set of referenced event state ids ≠ set of declared event state ids (plus '{FULFILLED_EVENT_STATE_LABEL}'):\n" +
            "Referenced           : " + str(sorted(self._referenced_event_stateids)) + "\n" +
            f"Defined + '{FULFILLED_EVENT_STATE_LABEL}': " + str(sorted(set(self._top.formal_contract.estates.keys()).union([FULFILLED_EVENT_STATE_LABEL])))
            )

        return self._top

    def contract_param(self, expr):
        self.assertOrSyntaxError( len(expr) == 5, expr, "Contract parameter dec should have form (name : type := term)" )
        return ContractParamDec(expr[0], expr[2], self.parse_term(expr[4]))

    def global_vars(self, l:SExpr):
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

    def claims(self, l:SExpr):
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

    def formal_contract(self, l:SExpr) -> FormalContract:
        estates: Dict[EventStateId, EventState] = dict()
        start_state: str   # EventState id
        x: SExpr

        for x in l[1:]:
            assert len(x) >= 2
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if head(START_STATE_LABEL):
                self.assertOrSyntaxError( len(x) == 2, l, "StartState declaration S-expression should have length 2")
                id = caststr(x[1])
                start_state = id
                self._referenced_event_stateids.add(id)

            # elif head(EVENT_STATES_SECTION_LABEL):
            #     # DEPRECATED
            #     event_state_decls = x[1:]
            #     estates = {caststr(esd[0]): self.event_state(cast(SExpr, esd)) for esd in event_state_decls}

            elif head(EVENTSTATE_LABEL) or head(ACTIONSTATE_LABEL):
                estate_id : str
                estate_data : SExpr
                if isinstance(x[1], str):
                    # e.g. (Event&State SomethingHappens ...)
                    estate_id = x[1]
                    estate_data = x.tillEnd(2)
                    estates[estate_id] = self.event_state(estate_id, None, estate_data, head(ACTIONSTATE_LABEL))
                else:
                    # e.g. (Event&State (SomethingHappens param&sort1 param&sort2) ...)
                    estate_id = caststr(x[1][0])
                    estate_params = castse(x[1][1:])
                    estate_data = castse(x.tillEnd(2))
                    estates[estate_id] = self.event_state(estate_id, estate_params, estate_data, head(ACTIONSTATE_LABEL))
            else:
                self.syntaxError(l, f"Unrecognized head {x[0]}")

        return FormalContract(caststr(l[0][1]), estates, start_state)

    def event_state(self, es_id:EventStateId, params:Optional[SExpr], rest:SExpr, is_action_state:bool) -> EventState:
        es = EventState(es_id, is_action_state)
        es.connections_by_role = dict()
        x: SExpr
        if isinstance(params, SExpr):
            es.params = self.event_state_params(cast(List[List[str]],params))
        for x in rest:
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if head(EVENT_STATE_DESCRIPTION_LABEL):
                es.description = caststr(x[1][1]) # extract from STRLIT expression

            elif head(EVENT_STATE_PROSE_REFS_LABEL):
                es.prose_refs = cast(List,x[1:]).copy()

            elif head(CODE_BLOCK_LABEL):
                es.code_block = self.code_block(cast(List[SExpr],x[1:]), es)

            elif head(OUT_TRANSITIONS_LABEL):
                if isinstance(x[1],SExpr) and isinstance(x[1][0],str) and (x[1][0] == 'guardsDisjointExhaustive' or x[1][0] == 'deadlinesDisjointExhaustive'):
                    x = x[1]
                    print("TODO: guardsDisjointExhaustive etc")
                connections = castse(x.tillEnd(1))
                for connection in connections:
                    # Ah, this has actually changed substantially. There are no longer actor blocks.

                    deontic_guard: Term = None
                    if connection[0] == 'if':
                        deontic_guard = self.parse_term(connection[1], es)
                        connection = connection[2]

                    print(connection)
                    actor_id = caststr(connection[0])
                    deontic_keyword = caststr(connection[1])
                    rest2 : SExpr = connection.tillEnd(2)

                    # try:
                    #     if deontic_keyword in DEONTIC_GUARD_MODALITIES:
                    #         deontic_guard = self.parse_term(cast(SExprOrStr,connection[2]))
                    #         rest2 = castse(connection[3:])
                    #     else:
                    #         deontic_guard = None
                    #         rest2 = castse(connection[2:])
                    # except Exception as e:
                    #     logging.error("Problem possibly with deontic_keyword: " + str(deontic_keyword))
                    #     raise e

                    if actor_id not in es.connections_by_role:
                        es.connections_by_role[actor_id] = set()

                    es.connections_by_role[actor_id].add(
                        self.transition_clause(rest2, es, actor_id, deontic_keyword, deontic_guard)
                    )

        return es

    def event_state_params(self, parts:List[List[str]]) -> ParamsDec:
        pdec : List[str]
        for pdec in parts:
            assert len(pdec) == 3, f"Expected [<param name str>, ':', SORTstr] but got {pdec}"
            self._top.sorts.add(pdec[2])

        rv = {pdec[0]:pdec[2] for pdec in parts}
        # logging.info(str(rv))
        return rv

    def code_block(self, statements:List[SExpr], es:EventState) -> CodeBlock:
        return CodeBlock([self.code_block_statement_dispatch(x,es) for x in statements])

    def code_block_statement_dispatch(self, statement:SExpr, es:EventState) -> CodeBlockStatement:
        try:
            if statement[0] == 'conjecture':
                self.assertOrSyntaxError( len(statement) == 2, statement, "CodeBlock conjecture expression should have length 2")
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
                if varname in es.local_vars:
                    self.syntaxError(statement, "Redeclaration of local variable")
                es.local_vars[varname] = lvd
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

    def parse_term(self, x:Union[str,SExpr], event_state:Optional[EventState] = None) -> Term:
        if isinstance(x,str):
            if x in self._top.global_var_decs:
                return GlobalVar(self._top.global_var_decs[x])
            if event_state and (x in event_state.local_vars):
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

    def transition_clause(self, trans_spec:SExpr, src_es:EventState, actor_id:ActorId,
                          deontic_modality: Optional[DeonticKeyword] = None, deontic_guard: Optional[Term] = None) -> TransitionClause:
        # assert len(trans_spec) >= 3, f"See src EventState {src_es_id} actor section {actor_id} trans_spec {trans_spec}"
        self.assertOrSyntaxError(isinstance(trans_spec[0], SExpr), trans_spec)
        dest_id : str = caststr(trans_spec[0][0])
        self._referenced_event_stateids.add(dest_id)
        src_es_id = src_es.name
        tc = TransitionClause(src_es_id, dest_id, actor_id, deontic_modality, deontic_guard)
        tc.args = castse(trans_spec[0][1:])

        assert 'where' not in trans_spec

        done_with_deadline = False
        if len(trans_spec) > 1:
            if trans_spec[1] in DEADLINE_KEYWORDS:
                tc.deadline_clause = self.parse_term(trans_spec[1], src_es)
                done_with_deadline = True

            if not done_with_deadline:
                if len(trans_spec[1]) > 0 and trans_spec[1][0] in DEADLINE_OPERATORS:
                    tc.deadline_clause = self.parse_term(trans_spec[1], src_es)

        return tc

    # def nonactor_transition_clause(self,trans_spec, src_es_id:EventStateId) -> TransitionClause:
    #     try:
    #         dest_id: str = trans_spec[0]
    #         self._referenced_event_stateids.add(dest_id)
    #         tc = TransitionClause(src_es_id, dest_id, NONACTION_BLOCK_LABEL, None)
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


EXAMPLES = (
    # '../examplesLSM4/hvitved_printer.LSM',
    # '../examplesLSM4/hvitved_lease.LSM',
    '../examplesLSM4/monster_burger.LSM',
    # '../examplesLSM4/SAFE.LSM',
    # '../examplesLSM4/hvitved_master_sales_agreement_simplified.LSM',
    # '../examplesLSM4/hvitved_master_sales_agreement_full.LSM',
    # '../examplesLSM4/hvitved_master_sales_agreement_full_with_ids.LSM',
    # '../examplesLSM4/hvitved_instalment_sale.LSM'
)


if __name__ == '__main__':
    import sys

    logging.basicConfig(
        format="[%(levelname)s] %(funcName)s: %(message)s",
        level=logging.INFO )

    if 'test' in sys.argv:
        import doctest
        doctest.testmod()

    if 'examples' in sys.argv:
        for path in EXAMPLES:
            if 'printSExpr' in sys.argv:
                print("\nLooking at file " + path + ":\n")
            parsed = parse_file(path)
            if 'printSExpr' in sys.argv:
                print(prettySExprStr(parsed))

            assembler = Assemble(path)
            prog : LSMTop = assembler.top(parsed)

            if 'printPretty' in sys.argv:
                print(prog)

            if 'dot' in sys.argv:
                contractToDotFile(prog)


# prog = Assemble().top(parse_file(EXAMPLES[0]))