import logging
from parse_sexpr import parse_file, prettySExprStr
from rich_model_from_sexpr import LSMConstructor
from model.LSMContract import LSMContract

EXAMPLES_ROOT = "./"

EXAMPLES = map( lambda x: EXAMPLES_ROOT + x, (
    # '../examplesLSM4/hvitved_printer.LSM',
    # '../examplesLSM4/hvitved_lease.LSM',
    'examples/monster_burger.L4',
    # '../examplesLSM4/SAFE.LSM',
    # '../examplesLSM4/hvitved_master_sales_agreement_simplified.LSM',
    # '../examplesLSM4/hvitved_master_sales_agreement_full.LSM',
    # '../examplesLSM4/hvitved_master_sales_agreement_full_with_ids.LSM',
    # '../examplesLSM4/hvitved_instalment_sale.LSM'
))



if __name__ == '__main__':
    import sys
    print(parse_file)

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

            assembler = LSMConstructor(path)
            prog : LSMContract = assembler.top(parsed)

            if 'printPretty' in sys.argv:
                print(prog)

            if 'dot' in sys.argv:
                contractToDotFile(prog)


    logging.warning("""
    Todo:
        enabled guards         
    """
)

# prog = LSMConstructor().top(parse_file(EXAMPLES[0]))