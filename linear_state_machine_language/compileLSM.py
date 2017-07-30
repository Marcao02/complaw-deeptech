# from typing import Union, List, Dict, Any, Tuple
from typing import Set
# from L4_util_constants_types import *

from parse_sexpr import parse, pretty
import logging

from LSMTop import *

from state_diagram_generation import contractToDotFile

SExpr = List[Any]



class Assemble:
    def __init__(self):
        self._top : L4Top = L4Top()
        self._referenced_event_stateids: Set[EventStateId] = set()

    def top(self, l:SExpr):
        x : SExpr
        for x in l:
            assert len(x) >= 2
            x0 = x[0]
            # print(x)
            if streqci(x0, GLOBAL_VARS_SECTION_LABEL):
                self._top.global_vars = self.global_vars(x[1:])
            elif streqci(x0, CLAIMS_SECTION_LABEL):
                self._top.claims = self.claims(x[1:])
            elif streqci(x0, ACTORS_SECTION_LABEL):
                self._top.actors = self.actors(x[1:])
            elif streqci(x0, PROSE_CONTRACT_SECTION_LABEL):
                self._top.prose_contract = self.prose_contract(x[1:])
            elif streqci(x0, FORMAL_CONTRACT_SECTION_LABEL):
                self._top.formal_contract = self.formal_contract(x[1:])
            elif streqci(x0, DOT_FILE_NAME_LABEL):
                self._top.dot_file_name = x[1][1] # the extra [1] is because its parse is of the form ['STRLIT', 'filename']
            elif streqci(x0, IMG_FILE_NAME_LABEL):
                self._top.img_file_name = x[1][1] # the extra [1] is because its parse is of the form ['STRLIT', 'filename']


        assert self._referenced_event_stateids.union(['Start']) == set(self._top.formal_contract.estates.keys()).union(["Fulfilled"]), (
            "Set of referenced event state ids (plus 'Start') ≠ set of declared event state ids (plus 'Fulfilled'):\n" +
            "Referenced + 'Start' : " + str(sorted(self._referenced_event_stateids.union(['Start']))) + "\n" +
            "Defined + 'Fulfilled': " + str(sorted(set(self._top.formal_contract.estates.keys()).union(["Fulfilled"])))
        )

        return self._top

    def global_vars(self, l:SExpr):
        # logging.info(l)
        rv = dict()
        for dec in l:
            if dec[0] in VARIABLE_MODIFIERS:
                self._top.sorts.add(dec[3])
                rv[dec[1]] = GlobalVar(dec[1], dec[3], dec[5], dec[0])
            else:
                self._top.sorts.add(dec[2])
                rv[dec[1]] = GlobalVar(dec[0], dec[2], dec[4])
        # logging.info(str(rv))
        return rv

    def claims(self, l:SExpr):
        rv = [ContractClaim(x) for x in l]
        # logging.info(str(rv))
        return rv

    def actors(self, l:SExpr):
        # logging.info(str(l))

        if isinstance(l[0], str):
            return l.copy()
        else:
            # maybe they're wrapped:
            assert len(l) == 1
            return self.actors(l[0])

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
        es.params = self.event_state_params(l[1])
        if len(l) == 4:
            es.prose_refs = l[2].copy()
            blocks = l[3]
        else:
            es.prose_refs = []
            blocks = l[2]

        es.proper_actor_blocks = dict()
        for block in blocks:
            if streqci(block[0], CODE_BLOCK_LABEL):
                es.code_block = self.code_block(block[1:])
            elif streqci(block[0], NONACTION_BLOCK_LABEL):
                es.nonactor_block = self.nonactor_block(block[1:], es_id)
            else:
                actor_id = block[0]
                es.proper_actor_blocks[actor_id] = self.actor_block(block[1:], es_id, actor_id)
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
        if statement[0] == 'conjecture' and statement[2] == '=':
            return AssertionEqualsStatement(statement[1], statement[3])
        elif statement[1] == '≔' or statement[1] == "=":
            return VarAssignStatement(statement[0], statement[2])
        return None # not reachable


    def actor_block(self, trans_specs:SExpr, src_esid: EventStateId, actor_id:ActorId) -> ActorBlock:
        return ActorBlock({tcs[0]: self.transition_clause(tcs, src_esid, actor_id) for tcs in trans_specs}, actor_id)

    def nonactor_block(self, trans_specs, src_esid: EventStateId) -> NonactorBlock:
        return NonactorBlock({tcs[0]: self.nonactor_transition_clause(tcs, src_esid) for tcs in trans_specs})

    def transition_clause(self, trans_spec:SExpr, src_es_id:EventStateId, actor_id:ActorId) -> TransitionClause:
        assert len(trans_spec) >= 3, trans_spec
        dest_id : str = trans_spec[0]
        self._referenced_event_stateids.add(dest_id)
        tc = TransitionClause(src_es_id, dest_id, actor_id)
        tc.args = trans_spec[1]
        if trans_spec[2] == 'by' or trans_spec[2] == 'on':
            tc.deadline = trans_spec[2:4]
        else:
            assert streqci(trans_spec[2],"immediately"), trans_spec[2]
            tc.deadline = trans_spec[2:4]
        return tc

    def nonactor_transition_clause(self,trans_spec, src_es_id:EventStateId) -> TransitionClause:
        dest_id: str = trans_spec[0]
        self._referenced_event_stateids.add(dest_id)
        tc = TransitionClause(src_es_id, dest_id, NONACTION_BLOCK_LABEL)
        tc.args = trans_spec[1]
        if len(trans_spec) > 2:
            if trans_spec[2] == 'by' or trans_spec[2] == 'on':
                tc.deadline = trans_spec[2:4]
            else:
                assert streqci(trans_spec[2], "immediately"), trans_spec[2]
                tc.deadline = trans_spec[2:4]
            return tc

        return tc


EXAMPLES = (
    'examples/hvitved_printer.LSM',
    'examples/hvitved_lease.LSM'
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
                print(parsed)
                print(pretty(parsed))
            assembler = Assemble()
            prog : L4Top = assembler.top(parsed)
            contractToDotFile(prog)


# prog = Assemble().top(parse_file(EXAMPLES[0]))