from test import test_parser
from src.independent.typing_imports import *
from src.model.L4Contract import L4Contract
from src.state_diagram_generation import contractToDotFile



def main(examples:Dict[str,L4Contract], verbose=True):
    for examplekey in examples:
        prog = examples[examplekey]
        contractToDotFile(prog, "examples/out_graphviz", use_filename=True, verbose=verbose)

def cli(sys_argv:List[str]):
    main(test_parser.main(keep=True, verbose=False), verbose=False)

if __name__ == '__main__':
    import sys
    cli(sys.argv)