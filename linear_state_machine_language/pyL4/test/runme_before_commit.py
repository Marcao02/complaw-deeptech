import os
import sys

from src.independent.util import print_all_todos

tests_to_run = {
    'typechecker',
    'smt',
    'graphviz',
    'prettyprint',
    'interpreter',
}

def runit(s, optional_s=""):
    line2 = f"Running {optional_s} `{s}`"
    print(f"\n{'='*len(line2)}\n{line2}")
    os.system(s)

if not "onlytc" in sys.argv and not "tconly" in sys.argv:
    import test_parser

    assert test_parser.EXAMPLES_FULL_SIZE == len(test_parser.EXAMPLES), "Some entries of cli.EXAMPLES are commented out, or you need to increase cli.EXAMPLES_FULL_SIZE"

    if "print" in sys.argv:
        print(f"\n============================================\nRunning `test_parser.py` with args `examples printPretty printSExpr dot`")
        test_parser.main("examples printPretty printSExpr dot")
        # runit("python3.6 src.parse_to_model_cli.py examples printPretty printSExpr dot")
    else:
        print(f"\n============================================\nRunning `test_parser.py` with args `examples`")
        test_parser.main("examples")
        # runit("python3.6 src.parse_to_model_cli.py examples")

    progs = test_parser.main(keep=True)

    if 'interpreter' in tests_to_run:
        import test_interpreter
        assert test_interpreter.EXAMPLES_FULL_SIZE == len(test_interpreter.EXAMPLES_TO_RUN), "Some entries of test_interpreter.EXAMPLES_TO_RUN are probably commented out"
        test_interpreter.main(progs)

    if 'typechecker' in tests_to_run:
        import test_typechecker
        test_typechecker.main(progs)

    if 'smt' in tests_to_run:
        import test_smt
        test_smt.main(progs)

    if 'graphviz' in tests_to_run:
        import test_graphviz
        test_graphviz.main(progs)

    if 'prettyprint' in tests_to_run:
        import test_prettyprint
        test_prettyprint.main(progs)

    print_all_todos()

# runit("export MYPYPATH=.; mypy --ignore-missing-imports src", "typechecker")
runit("export MYPYPATH=.; mypy src", "typechecker")



