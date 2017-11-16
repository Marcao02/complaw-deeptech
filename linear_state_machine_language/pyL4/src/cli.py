import logging
from parse_sexpr import parse_file, prettySExprStr
from rich_model_from_sexpr import L4ContractConstructor
from model.L4Contract import L4Contract
from model.util import writeFile, writeReadOnlyFile

# '../examplesLSM4/hvitved_printer.LSM',
# '../examplesLSM4/hvitved_lease.LSM',
# '../examplesLSM4/SAFE.LSM',
# '../examplesLSM4/hvitved_master_sales_agreement_simplified.LSM',
# '../examplesLSM4/hvitved_master_sales_agreement_full.LSM',
# '../examplesLSM4/hvitved_master_sales_agreement_full_with_ids.LSM',
# '../examplesLSM4/hvitved_instalment_sale.LSM'
EXAMPLES = ['monster_burger.L4']

EXAMPLES_SEXPR_ROOT = "./examples_sexpr/"
EXAMPLE_SEXPR_ = map( lambda x: EXAMPLES_SEXPR_ROOT + x, EXAMPLES )

EXAMPLES_UNPARSED_ROOT = "./examples_unparsed/"


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
        for filename in EXAMPLES:
            in_path = EXAMPLES_SEXPR_ROOT + filename
            if 'printSExpr' in sys.argv:
                print("\nLooking at file " + filename + ":\n")
            parsed = parse_file(in_path)
            if 'printSExpr' in sys.argv:
                print(prettySExprStr(parsed))

            assembler = L4ContractConstructor()
            prog : L4Contract = assembler.top(parsed)

            if 'printPretty' in sys.argv:
                unparsed = str(prog)
                print(unparsed)
                writeReadOnlyFile(EXAMPLES_UNPARSED_ROOT + filename, unparsed)

            # if 'dot' in sys.argv:
            #     contractToDotFile(prog)

    logging.warning("""
    Todo:        
        typecheck!                        
        translate more examples
        deadlinesDisjointExhaustive and guardsDisjointExhaustive
        
        fn symbol types
        interpreter            
        
        graphviz generation
    """
)

# prog = L4ContractConstructor().top(parse_file(EXAMPLES[0]))