from test import test_parser

from src.nlg.english_gen_prototype import gen_english
from test.active_examples import EXAMPLES_HTML_ROOT
from src.independent.typing_imports import *
from src.model.L4Contract import L4Contract


def main(examples:Dict[str,L4Contract]):
    for examplekey in examples:
        if "SAFE_" in examplekey:
            prog = examples[examplekey]
            assert prog.must_eliminated
            assert not prog.local_vars_eliminated
            assert not prog.if_then_else_terms_eliminated
            gen_english(prog, EXAMPLES_HTML_ROOT + examplekey + ".html")

def cli(sys_argv:List[str]):
    main(test_parser.main(keep=True, verbose=False))

if __name__ == '__main__':
    import sys
    cli(sys.argv)
