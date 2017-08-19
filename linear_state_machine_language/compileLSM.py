import logging

from LSMTop import *
from parse_sexpr import prettySExprStr, SExpr, SExprOrStr, castse, STRING_LITERAL_MARKER, parse_file
from state_diagram_generation import contractToDotFile
from util import streqci, list_split, caststr, isFloat
from typing import Tuple, cast, Optional
from LSMStatements import Term, ContractParamDec

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
            rem = cast(SExpr,x[1:])
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)

            if   head( GLOBAL_VARS_SECTION_LABEL ):
                assert isinstance(x,SExpr), type(x)
                self._top.global_var_decs = self.global_vars(cast(SExpr, rem))
            elif head( CONTRACT_PARAMETERS_SECTION_LABEL ):
                self._top.contract_params = {expr[0] : self.contract_param(expr) for expr in rem}
            elif head( CLAIMS_SECTION_LABEL ):
                self._top.claims = self.claims(cast(SExpr,rem))
            elif head( CLAIMS_SECTION_LABEL ):
                self._top.claims = self.claims(cast(SExpr,rem))
            elif head( ACTORS_SECTION_LABEL ):
                self._top.actors = self.actors(cast(List[str],rem))
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
                modifiers = []
                while True:
                    if dec[i] in VARIABLE_MODIFIERS:
                        modifiers.append(dec[i])
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
                rv[name] = GlobalVarDec(name, sort, initval, castse(modifiers))
            except Exception as e:
                logging.error("Problem processing " + str(dec))
                raise e

        # logging.info(str(rv))
        return rv

    def claims(self, l:SExpr):
        rv = [ContractClaim(x) for x in l]
        # logging.info(str(rv))
        return rv

    def actors(self, l:List[str]) -> List[str]:
        # logging.info(str(l))
        self.assertOrSyntaxError(all(isinstance(x, str) for x in l), castse(l), "Actors declaration S-expression should have the form (Actors Alice Bob)")
        return cast(List,l).copy()

    def prose_contract(self, l: List[List[str]]) -> ProseContract:
        rv = {x[0]: x[1] for x in l}
        # logging.info(str(rv))
        return rv

    def formal_contract(self, l:SExpr) -> FormalContract:
        estates: Dict[EventStateId, EventState] = dict()
        start_state: str   # EventState id

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

            elif head(EVENT_STATES_SECTION_LABEL):
                event_state_decls = x[1:]
                estates = {caststr(esd[0]): self.event_state(cast(SExpr, esd)) for esd in event_state_decls}

        return FormalContract(caststr(l[0][1]), estates, start_state)

    def event_state(self, l:SExpr) -> EventState:
        es_id : EventStateId = caststr(l[0])
        es = EventState(es_id)
        es.proper_actor_blocks = dict()

        es.params = self.event_state_params(cast(List[str],l[1]))

        for x in l[2:]:
            def head(constant:str) -> bool:
                nonlocal x
                return streqci(x[0], constant)
            if head(EVENT_STATE_DESCRIPTION_LABEL):
                es.description = caststr(x[1][1]) # extract from STRLIT expression
            elif head(EVENT_STATE_PROSE_REFS_LABEL):
                es.prose_refs = cast(List,x[1:]).copy()
            elif head(CODE_BLOCK_LABEL):
                es.code_block = self.code_block(cast(List[SExpr],x[1:]), es)
            elif head(NONACTION_BLOCK_LABEL):
                es.nonactor_block = self.nonactor_block(castse(x[1:]), es_id)
            elif head(EVENT_STATE_ACTOR_BLOCKS_LABEL):
                actorblocks = castse(x[1:])
                for actorblock in actorblocks:
                    actor_id = caststr(actorblock[0])
                    deontic_keyword = caststr(actorblock[1])
                    rest : SExpr
                    deontic_guard : Term
                    try:
                        if deontic_keyword in DEONTIC_GUARD_MODALITIES:
                            deontic_guard = self.parse_term(cast(SExprOrStr,actorblock[2]))
                            rest = castse(actorblock[3:])
                        else:
                            deontic_guard = None
                            rest = castse(actorblock[2:])
                    except Exception as e:
                        logging.error("Problem possibly with deontic_keyword: " + str(deontic_keyword))
                        raise e

                    if actor_id not in es.proper_actor_blocks:
                        es.proper_actor_blocks[actor_id] = set()

                    es.proper_actor_blocks[actor_id].update(
                        self.actor_block(cast(List[SExpr],rest), es_id, actor_id, deontic_keyword, deontic_guard))


        return es

    def event_state_params(self, params:List[str]) -> ParamsDec:
        parts = list_split(',', params)

        pdec : List[str]
        for pdec in parts:
            assert len(pdec) == 3, f"Expected [VARstr, ':', SORTstr] but got {pdec}"
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
            if isFloat(x):
                return float(x)
            if x == 'false':
                return False
            if x == 'true':
                return True
            if x == 'never':
                return DeadlineLit(x)
            logging.warning('Unrecognized atom: ' + x + '. Treating as deadline literal.')
            return DeadlineLit(x)
        elif isinstance(x,list) and len(x) == 2 and x[0] == STRING_LITERAL_MARKER:
            return StringLit(caststr(x[1]))
        else:
            pair = try_parse_infix(x) or try_parse_prefix(x)
            if not pair:
                logging.error("Didn't recognize function symbol in: " + str(x))
                self.syntaxError(x)
            return FnApp(pair[0], [self.parse_term(arg) for arg in pair[1]])
        # return term

    def actor_block(self, trans_specs:List[SExpr], src_esid: EventStateId,
                    actor_id:ActorId,
                    deontic_keyword: DeonticKeyword,
                    deontic_guard:Term) -> Set[TransitionClause]:
        return {self.transition_clause(tcs, src_esid, actor_id, deontic_keyword, deontic_guard) for tcs in trans_specs}

    def nonactor_block(self, trans_specs:SExpr, src_esid: EventStateId) -> Set[TransitionClause]:
        assert trans_specs is not None
        return {self.nonactor_transition_clause(tcs, src_esid) for tcs in trans_specs}

    def transition_clause(self, trans_spec:SExpr, src_es_id:EventStateId, actor_id:ActorId,
                          deontic_modality: Optional[DeonticKeyword] = None, deontic_guard: Optional[Term] = None) -> TransitionClause:
        assert len(trans_spec) >= 3, f"See src EventState {src_es_id} actor section {actor_id} trans_spec {trans_spec}"
        dest_id : str = caststr(trans_spec[0])
        self._referenced_event_stateids.add(dest_id)
        tc = TransitionClause(src_es_id, dest_id, actor_id, deontic_modality, deontic_guard)
        tc.args = castse(trans_spec[1])

        ind = 2
        if 'where' in trans_spec[2:]:
            ind = trans_spec.index('where')
            tc.where_clause = self.parse_term(castse(trans_spec[ind+1]))
            ind = ind + 2

        for deadline_keyword in DEADLINE_OPERATORS:
            if deadline_keyword in trans_spec[2:]:
                ind = trans_spec.index(deadline_keyword)
                tc.deadline_clause = castse(trans_spec[ind:ind+2])
                ind = ind + 2
                # TODO
                # tc.deadline_clause = DeadlineClause(trans_spec[ind], trans_spec[ind + 1:])

        # TODO conditions should be obsolete
        tc.conditions = castse(trans_spec[ind:])
        # print(tc.conditions)
        return tc

    def nonactor_transition_clause(self,trans_spec, src_es_id:EventStateId) -> TransitionClause:
        try:
            dest_id: str = trans_spec[0]
            self._referenced_event_stateids.add(dest_id)
            tc = TransitionClause(src_es_id, dest_id, NONACTION_BLOCK_LABEL, None)
            tc.args = trans_spec[1]
            if len(trans_spec) > 2:
                if trans_spec[2] in DEADLINE_OPERATORS:
                    tc.conditions = trans_spec[2:4]
                else:
                    # assert trans_spec[2] in DEADLINE_KEYWORDS, trans_spec[2] + ' is not a deadline keyword'
                    tc.conditions = trans_spec[2:4]
                return tc
            return tc
        except Exception:
            logging.error(f"Problem processing {src_es_id} trans: " + str(trans_spec))
            return None

def try_parse_infix(lst:SExpr) -> Tuple[str, SExpr]:
    try:        
        if len(lst) == 3 and isinstance(lst[1],str):
            symb : str = lst[1]
            if symb in INFIX_FN_SYMBOLS:
                return symb, castse([lst[0]] + lst[2:])
        return None
    except:
        print("try_parse_infix")
        return None


def try_parse_prefix(lst:SExpr) -> Tuple[str, SExpr]:

    if isinstance(lst[0],str):
        symb = lst[0]
        if symb in PREFIX_FN_SYMBOLS:
            if len(lst) == 1:
                return symb, castse([])
            else:
                return symb, castse(lst[1:])
    return None

EXAMPLES = (
    'examples/hvitved_printer.LSM',
    'examples/hvitved_lease.LSM',
    'examples/monster_burger.LSM',
    'examples/SAFE.LSM',
    'examples/hvitved_master_sales_agreement_simplified.LSM',
    'examples/hvitved_master_sales_agreement_full.LSM',
    'examples/hvitved_master_sales_agreement_full_with_ids.LSM',
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