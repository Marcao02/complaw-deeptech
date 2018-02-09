import logging
from typing import List

from src.hard_correctness_checks.normal_forms import eliminate_local_vars
from src.independent.util import writeReadOnlyFile
from src.parse_to_model.sexpr_to_L4Contract import L4ContractConstructor
from src.correctness_checks import test_fns
from src.independent.parse_sexpr import parse_file, prettySExprStr
from src.state_diagram_generation import contractToDotFile

EXAMPLES_SEXPR_ROOT = "./examples/src_sexpr/"
EXAMPLES_UNPARSED_ROOT = "./examples/out_prettyprinted/"

EXAMPLES = [
    'toy_and_teaching/test_local_vars.l4',
    'toy_and_teaching/minimal_future-actions.l4',
    'toy_and_teaching/minimal_future-actions2.l4',
    'toy_and_teaching/collatz.l4',
    'toy_and_teaching/monster_burger_program_only.l4',
    'toy_and_teaching/partner_assignment_relievable_obligations.l4',
    'toy_and_teaching/hvitved_modeling_prohibition_trivial_nda.l4',

    'from_academic_lit/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.l4',
    'from_academic_lit/hvitved_master_sales_agreement_full_without_future_obligations.l4',
    'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',
    'from_academic_lit/hvitved_lease.l4',
    'from_academic_lit/hvitved_printer.l4',
    'from_academic_lit/prisacariu_schneider_abdelsadiq_Internet_provision_with_renew.l4',

    'serious/SAFE.l4',
    'serious/SAFE_2_liq_eventtypes.l4',
    'serious/KISS.l4'
]
EXAMPLES_FULL_SIZE = 16

# so can run it as a library too, which respects exceptions
def main(sys_argv:List[str]):

    logging.basicConfig(
        format="[%(levelname)s] %(funcName)s: %(message)s",
        level=logging.INFO )

    if 'test' in sys_argv:
        import doctest
        doctest.testmod()

    if 'examples' in sys_argv:
        for filename in EXAMPLES:
            print(f"\n---------------------------------\nExample {filename}:")
            in_path = EXAMPLES_SEXPR_ROOT + filename
            if 'printSExpr' in sys_argv:
                print("\nLooking at file " + filename + ":\n")
            parsed = parse_file(in_path)
            if 'printSExpr' in sys_argv:
                print(prettySExprStr(parsed))

            assembler = L4ContractConstructor(filename)
            prog = assembler.mk_l4contract(parsed)

            # eliminate_local_vars(prog)

            # print( prog.action_ids() )
            # print( prog.section_ids() )
            # print(prog.max_section_id_len, prog.max_action_id_len)

            if 'printPretty' in sys_argv:
                prettyprinted = str(prog)
                print(prettyprinted)
                writeReadOnlyFile(EXAMPLES_UNPARSED_ROOT + filename, prettyprinted)

            if 'dot' in sys_argv:
                contractToDotFile(prog, "examples/out_graphviz" , True, True)

            print("\n~~~~~~~~~~~~~~\nRunning correctness checks...")
            for check in test_fns:
                print()
                if check(assembler):
                    print( check.__name__ + " ok" )

if __name__ == '__main__':
    import sys
    main(sys.argv)


                        # from typing import TypeVar, Iterable, Dict, Callable, List
#
# from src.model.StateVarDec import StateVarDec
# from src.model.L4Contract import L4Contract
# T = TypeVar('T')
# def mkdict(iter : List[T], name:Callable[[T],str] = lambda x:x.name) -> Dict[str,T]:
#     return {name(x): x for x in iter}
# def gvardec(name: str, sort: str, initval: Optional[Term], modifier:List[str]) -> StateVarDec:
#     return StateVarDec(castid(GlobalVarDecId), sort, initval, modifier)
#
# c = L4Contract('monster burger')
# c.start_section_id = 'MonsterBurgerUncooked'
# c.global_var_decs = mkdict([
#     gvardec('challenge_endlimit_timestamp', 'Timestamp', None, ['writeonce']),
# 	gvardec('amount_owing', '$', 0, []),
# 	gvardec('amount_paid', '$', 0, [])
# ])