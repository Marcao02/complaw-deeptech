import test_parser

from src.nlg.english_gen_prototype import gen_english
from test.active_examples import EXAMPLES_UNPARSED_ROOT, EXAMPLES_HTML_ROOT
from src.independent.util_for_io import writeReadOnlyFile
from src.independent.typing_imports import *
from src.model.L4Contract import L4Contract


def main(examples:Dict[str,L4Contract]):
    for examplekey in examples:
        prog = examples[examplekey]
        prettyprinted = str(prog)
        writeReadOnlyFile(EXAMPLES_UNPARSED_ROOT + examplekey + ".out", prettyprinted)

        if "SAFE_nlg" in examplekey:
            gen_english(prog, EXAMPLES_HTML_ROOT + examplekey + ".html")

def cli(sys_argv:List[str]):
    main(test_parser.main(keep=True, verbose=False))

if __name__ == '__main__':
    import sys
    cli(sys.argv)
