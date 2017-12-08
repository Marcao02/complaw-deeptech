import logging
from typing import List

from src.parse_sexpr import parse_file, prettySExprStr
from src.sexpr_to_L4Contract import L4ContractConstructor
from src.correctness_checks import test_fns
from src.model.util import writeReadOnlyFile
from src.state_diagram_generation import contractToDotFile

EXAMPLES_SEXPR_ROOT = "./examples_sexpr/"
EXAMPLES_UNPARSED_ROOT = "./examples_prettyprinted_out/"

EXAMPLES = [
    # 'degenerate/minimal_future-actions.l4',
    # 'degenerate/minimal_future-actions2.l4',
    # 'degenerate/collatz.l4',
    # 'toy_and_teaching/monster_burger_program_only.l4',
    # 'from_academic_lit/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.l4',
    # 'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',
    # 'from_academic_lit/hvitved_lease.l4',
    'from_academic_lit/prisacariu_schneider_abdelsadiq_Internet_provision_with_renew.l4',
    # 'serious/SAFE.l4',
    # 'from_academic_lit/hvitved_printer.l4'
]
EXAMPLES_FULL_SIZE = 9

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

            # print( prog.action_ids() )
            # print( prog.section_ids() )
            # print(prog.max_section_id_len, prog.max_action_id_len)

            if 'printPretty' in sys_argv:
                prettyprinted = str(prog)
                print(prettyprinted)
                writeReadOnlyFile(EXAMPLES_UNPARSED_ROOT + filename, prettyprinted)

            if 'dot' in sys_argv:
                contractToDotFile(prog, "examples_graphviz_out" , True, True)

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
# from src.model.GlobalVarDec import GlobalVarDec
# from src.model.L4Contract import L4Contract
# T = TypeVar('T')
# def mkdict(iter : List[T], name:Callable[[T],str] = lambda x:x.name) -> Dict[str,T]:
#     return {name(x): x for x in iter}
# def gvardec(name: str, sort: str, initval: Optional[Term], modifier:List[str]) -> GlobalVarDec:
#     return GlobalVarDec(castid(GlobalVarDecId), sort, initval, modifier)
#
# c = L4Contract('monster burger')
# c.start_section_id = 'MonsterBurgerUncooked'
# c.global_var_decs = mkdict([
#     gvardec('challenge_endlimit_timestamp', 'Timestamp', None, ['writeonce']),
# 	gvardec('amount_owing', '$', 0, []),
# 	gvardec('amount_paid', '$', 0, [])
# ])