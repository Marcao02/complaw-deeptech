import test_parser
from test.active_examples import EXAMPLES_UNPARSED_ROOT
from src.independent.util_for_io import writeReadOnlyFile
from src.independent.typing_imports import *
from src.model.L4Contract import L4Contract


def main(examples:Dict[str,L4Contract]):
    for examplekey in examples:
        prog = examples[examplekey]
        prettyprinted = str(prog)
        writeReadOnlyFile(EXAMPLES_UNPARSED_ROOT + examplekey + ".out", prettyprinted)

def cli(sys_argv:List[str]):
    main(test_parser.main(keep=True, verbose=False))

if __name__ == '__main__':
    import sys
    cli(sys.argv)
