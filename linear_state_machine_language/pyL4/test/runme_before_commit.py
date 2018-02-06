import os
import sys

from src.independent.util import print_all_todos


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

    import test_interpreter
    assert test_interpreter.EXAMPLES_FULL_SIZE == len(test_interpreter.EXAMPLES_TO_RUN), "Some entries of test_interpreter.EXAMPLES_TO_RUN are probably commented out"

    import test_typechecker
    test_interpreter.main(sys.argv)
    test_typechecker.main(sys.argv)

    print_all_todos()

# runit("export MYPYPATH=.; mypy --ignore-missing-imports src", "typechecker")
runit("export MYPYPATH=.; mypy src", "typechecker")



