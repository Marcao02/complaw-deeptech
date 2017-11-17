import logging
from parse_sexpr import parse_file, prettySExprStr
from rich_model_from_sexpr import L4ContractConstructor
from model.L4Contract import L4Contract
from model.util import writeReadOnlyFile
from state_diagram_generation import contractToDotFile

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

            if 'dot' in sys.argv:
                contractToDotFile(prog, "examples_graphviz_gen" , True)

            # for x in prog.sections_by_id:
            #     print(x)

    logging.warning("""
    Todo:        
        currently there's an inconsistecy that some Connections have a section id as their action id.
            idea 1: Every connection is a ConnectionToAction or a ConnectionToSection.
                    Then I would only need Env for proper environment events, as opposed to for any section-to-section connection.
            idea 2: AfterA is an action for every A and EnterS is a Section for every S. These get created only when used.
            idea 3 (can be combined with previous): Similar to idea 1, but still in the formal model go through the trivial EnterS action.
                    In other words, ConnectionToSection is an L4 feature, not an LSM feature.  
                    
        translate more examples
        deadlinesDisjointExhaustive and guardsDisjointExhaustive
        
        fn symbol types
        interpreter                            
    """
)

# prog = L4ContractConstructor().top(parse_file(EXAMPLES[0]))