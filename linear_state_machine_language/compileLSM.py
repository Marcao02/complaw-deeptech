import logging

from LSMTop import *
from parse_sexpr import parse, pretty
from state_diagram_generation import contractToDotFile
from util import streqci, list_split
from constants_and_defined_types import SExpr
from typing import Tuple, cast


class Assemble:
    def __init__(self):
        self._top : L4Top = L4Top()
        self._referenced_event_stateids: Set[EventStateId] = set()

    def top(self, l:SExpr):
        x : SExpr
        for x in l:
            assert len(x) >= 2, "Problem top-level: " + str(x)
            x0 = x[0]
            # print(x)
            if streqci(x0, GLOBAL_VARS_SECTION_LABEL):
                self._top.global_vars = self.global_vars(x[1:])
            elif streqci(x0, CLAIMS_SECTION_LABEL):
                self._top.claims = self.claims(x[1:])
            elif streqci(x0, ACTORS_SECTION_LABEL):
                self._top.actors = self.actors(cast(List[str],x[1:]))
            elif streqci(x0, PROSE_CONTRACT_SECTION_LABEL):
                self._top.prose_contract = self.prose_contract(cast(List[List[str]],x[1:]))
            elif streqci(x0, FORMAL_CONTRACT_SECTION_LABEL):
                self._top.formal_contract = self.formal_contract(x[1:])
            elif streqci(x0, DOT_FILE_NAME_LABEL):
                self._top.dot_file_name = x[1][1] # the extra [1] is because its parse is of the form ['STRLIT', 'filename']
            elif streqci(x0, IMG_FILE_NAME_LABEL):
                self._top.img_file_name = x[1][1] # the extra [1] is because its parse is of the form ['STRLIT', 'filename']

        if  self._referenced_event_stateids != set(self._top.formal_contract.estates.keys()).union([FULFILLED_EVENT_STATE_LABEL]):
            print(
            f"ISSUE\n:Set of referenced event state ids ≠ set of declared event state ids (plus '{FULFILLED_EVENT_STATE_LABEL}'):\n" +
            "Referenced           : " + str(sorted(self._referenced_event_stateids)) + "\n" +
            f"Defined + '{FULFILLED_EVENT_STATE_LABEL}': " + str(sorted(set(self._top.formal_contract.estates.keys()).union([FULFILLED_EVENT_STATE_LABEL])))
            )

        return self._top

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
                name = dec[i]
                sort = dec[i+2]
                self._top.sorts.add(sort)

                initval = None
                if i+3 < len(dec) and dec[i+3] == ':=':
                    initval = dec[i+4]
                rv[name] = GlobalVar(name, sort, initval, modifiers)
            except Exception:
                logging.error("Problem processing " + str(dec))

        # logging.info(str(rv))
        return rv

    def claims(self, l:SExpr):
        rv = [ContractClaim(x) for x in l]
        # logging.info(str(rv))
        return rv

    def actors(self, l:List[str]) -> List[str]:
        # logging.info(str(l))
        assert isinstance(l[0], str), str(l) + " should be a list of strings"
        return cast(List,l).copy()

    def prose_contract(self, l: List[List[str]]) -> ProseContract:
        rv = {x[0]: x[1] for x in l}
        # logging.info(str(rv))
        return rv

    def formal_contract(self, l:SExpr) -> FormalContract:
        fc = FormalContract(name=l[0])
        fc.params = dict()

        for x in l[1:]:
            assert len(x) >= 2
            x0 = x[0]
            if streqci(x0, START_STATE_LABEL):
                fc.start_state = x[1]
                self._referenced_event_stateids.add(x[1])

            if streqci(x0, CONTRACT_PARAMETERS_SECTION_LABEL):
                param_decls = x[1:]
                for pdecl in param_decls:
                    # param name -> sort
                    fc.params[pdecl[0]] = pdecl[1]

            elif streqci(x0, EVENT_STATES_SECTION_LABEL):
                event_state_decls = x[1:]
                fc.estates = {esd[0]: self.event_state(esd) for esd in event_state_decls}

        return fc

    def event_state(self, l:SExpr) -> EventState:
        es_id : EventStateId = l[0]
        es = EventState(es_id)
        es.proper_actor_blocks = dict()

        es.params = self.event_state_params(cast(List[str],l[1]))

        for x in l[2:]:
            x0 = x[0]
            if streqci(x0, EVENT_STATE_DESCRIPTION_LABEL):
                es.description = x[1]
            elif streqci(x0, EVENT_STATE_PROSE_REFS_LABEL):
                es.prose_refs = cast(List,x[1:]).copy()
            elif streqci(x0, CODE_BLOCK_LABEL):
                es.code_block = self.code_block(x[1:])
            elif streqci(x0, NONACTION_BLOCK_LABEL):
                es.nonactor_block = self.nonactor_block(x[1:], es_id)
            elif streqci(x0, EVENT_STATE_ACTOR_BLOCKS_LABEL):
                actorblocks = x[1:]
                for actorblock in actorblocks:
                    actor_id = actorblock[0]
                    deontic_keyword = actorblock[1]
                    try:
                        if deontic_keyword in DEONTIC_GUARD_MODALITIES:
                            guard = actorblock[2]
                            rest = actorblock[3:]
                        else:
                            guard = None
                            rest = actorblock[2:]
                    except Exception:
                        logging.error("Problem possibly with deontic_keyword: " + str(deontic_keyword))
                    if actor_id not in es.proper_actor_blocks:
                        es.proper_actor_blocks[actor_id] = set()
                    es.proper_actor_blocks[actor_id].update(self.actor_block(rest, es_id, actor_id, deontic_keyword, guard))


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

    def code_block(self, statements:SExpr) -> CodeBlock:
        return CodeBlock([self.code_block_statement_dispatch(x) for x in statements])

    def code_block_statement_dispatch(self, statement:SExpr) -> CodeBlockStatement:
        try:
            if statement[0] == 'conjecture':
                return InCodeConjectureStatement(cast(List,statement[1:]))
            else:
                assert len(statement) == 3, "As of now, every code block statement other than a conjecture should be a tripple: a :=, +=, or -= specifically. See\n" + str(statement)

                rhs = self.parse_term(statement[2])

                if statement[1] == ':=' or statement[1] == "=":
                    return VarAssignStatement(statement[0], rhs)
                elif statement[1] == '+=':
                    return IncrementStatement(statement[0], rhs)
                elif statement[1] == '-=':
                    return DecrementStatement(statement[0], rhs)
                else:
                    raise Exception
                return None # not reachable
        except Exception as e:
            logging.error(f"Problem with {statement}")
            raise e

    def parse_term(self, term:Union[str,SExpr]) -> Union[Term,str]:
        if isinstance(term,str):
            return term
        infixtry = try_parse_infix(term)
        if infixtry :
            return FnApp(infixtry[0], infixtry[1])
        prefixtry = try_parse_prefix(term)
        if prefixtry:
            return FnApp(prefixtry[0], prefixtry[1])
        return term




    def actor_block(self, trans_specs:SExpr, src_esid: EventStateId,
                    actor_id:ActorId,
                    deontic_keyword: DeonticKeyword, guard:SExpr) -> Set[TransitionClause]:
        return {self.transition_clause(tcs, src_esid, actor_id, deontic_keyword, guard) for tcs in trans_specs}

    def nonactor_block(self, trans_specs, src_esid: EventStateId,
                       guard: SExpr = None) -> Set[TransitionClause]:
        return {self.nonactor_transition_clause(tcs, src_esid, guard) for tcs in trans_specs}

    def transition_clause(self, trans_spec:SExpr, src_es_id:EventStateId, actor_id:ActorId,
                          deontic_modality: DeonticKeyword = None, guard: SExpr = None) -> TransitionClause:
        assert len(trans_spec) >= 3, f"See src EventState {src_es_id} actor section {actor_id} trans_spec {trans_spec}"
        dest_id : str = trans_spec[0]
        self._referenced_event_stateids.add(dest_id)
        tc = TransitionClause(src_es_id, dest_id, actor_id, deontic_modality, guard)
        tc.args = trans_spec[1]
        try:
            ind = trans_spec[2:].index('where')
            tc.where_clause = trans_spec[ind+1:]
        except ValueError:
            pass

        tc.conditions = trans_spec[2:]
        # print(tc.conditions)
        return tc

    def nonactor_transition_clause(self,trans_spec, src_es_id:EventStateId, guard:SExpr) -> TransitionClause:
        try:
            dest_id: str = trans_spec[0]
            self._referenced_event_stateids.add(dest_id)
            tc = TransitionClause(src_es_id, dest_id, NONACTION_BLOCK_LABEL, None, guard)
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

def try_parse_infix(lst:List[SExpr]) -> Tuple[str, SExpr]:    
    try:        
        if len(lst) == 3 and isinstance(lst[1],str):
            symb : str = lst[1]
            if symb in INFIX_FN_SYMBOLS:
                return symb, [lst[0]] + lst[2:]
        return None
    except:
        print("try_parse_infix")
        return None


def try_parse_prefix(lst:SExpr) -> Tuple[str, SExpr]:

    if isinstance(lst[0],str):
        symb = lst[0]
        if symb in PREFIX_FN_SYMBOLS:
            if len(lst) == 1:
                return symb, []
            else:
                return symb, lst[1:]
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


def parse_file(path):
    fil = open(path, 'r')
    parsed = parse(fil.read())
    fil.close()
    return parsed


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
            if 'print' in sys.argv:
                print("\nLooking at file " + path + ":\n")
            parsed = parse_file(path)
            if 'print' in sys.argv:
                # print(parsed)
                print(pretty(parsed))
                pass
            assembler = Assemble()
            prog : L4Top = assembler.top(parsed)
            contractToDotFile(prog)


# prog = Assemble().top(parse_file(EXAMPLES[0]))