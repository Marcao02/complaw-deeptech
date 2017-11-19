import logging
from parse_sexpr import parse_file, prettySExprStr
from sexpr_to_L4Contract import L4ContractConstructor
from correctness_checks import test_fns
from model.util import writeReadOnlyFile
from state_diagram_generation import contractToDotFile

import interpreter

# '../examplesLSM4/hvitved_printer.LSM',
# '../examplesLSM4/hvitved_lease.LSM',
# '../examplesLSM4/SAFE.LSM',
# '../examplesLSM4/hvitved_master_sales_agreement_simplified.LSM',
# '../examplesLSM4/hvitved_master_sales_agreement_full.LSM',
# '../examplesLSM4/hvitved_master_sales_agreement_full_with_ids.LSM',
# '../examplesLSM4/hvitved_instalment_sale.LSM'
EXAMPLES = [
    'collatz.l4',
    'monster_burger_program_only.l4',
    'hvitved_instalment_sale.l4',

]

EXAMPLES_SEXPR_ROOT = "./examples_sexpr/"
EXAMPLE_SEXPR_ = map( lambda x: EXAMPLES_SEXPR_ROOT + x, EXAMPLES )

EXAMPLES_UNPARSED_ROOT = "./examples_unparsed_gen/"


if __name__ == '__main__':
    import sys

    logging.basicConfig(
        format="[%(levelname)s] %(funcName)s: %(message)s",
        level=logging.INFO )

    if 'test' in sys.argv:
        import doctest
        doctest.testmod()

    if 'examples' in sys.argv:
        for filename in EXAMPLES:
            print(f"\n---------------------------------\nExample {filename}:")
            in_path = EXAMPLES_SEXPR_ROOT + filename
            if 'printSExpr' in sys.argv:
                print("\nLooking at file " + filename + ":\n")
            parsed = parse_file(in_path)
            if 'printSExpr' in sys.argv:
                print(prettySExprStr(parsed))

            assembler = L4ContractConstructor(filename)
            prog = assembler.l4contract(parsed)

            # print( prog.action_ids() )
            # print( prog.section_ids() )
            # print(prog.max_section_id_len, prog.max_action_id_len)

            if 'printPretty' in sys.argv:
                unparsed = str(prog)
                print(unparsed)
                writeReadOnlyFile(EXAMPLES_UNPARSED_ROOT + filename, unparsed)

            if 'dot' in sys.argv:
                contractToDotFile(prog, "examples_graphviz_gen" , True, True)

            print("\n~~~~~~~~~~~~~~\nRunning correctness checks...")
            for check in test_fns:
                print()
                if check(assembler):
                    print( check.__name__ + " ok" )

    # print("""\n\n
    # Todo:
    #     Unrecognized atoms
    #     translate more examples
    #         Hvitved master sales agreement
    #     deadlinesPartitionFuture and guardsDisjointExhaustive
    #
    # """)

# prog = L4ContractConstructor().top(parse_file(EXAMPLES[0]))