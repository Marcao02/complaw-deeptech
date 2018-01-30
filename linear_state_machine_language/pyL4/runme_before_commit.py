import os
import sys

from src.independent.util import print_all_todos


def runit(s, optional_s=""):
    line2 = f"Running {optional_s} `{s}`"
    print(f"\n{'='*len(line2)}\n{line2}")
    os.system(s)

if not "onlytc" in sys.argv and not "tconly" in sys.argv:
    from src.compiler import compiler_cli

    assert compiler_cli.EXAMPLES_FULL_SIZE == len(
        compiler_cli.EXAMPLES), "Some entries of cli.EXAMPLES are commented out, or you need to increase cli.EXAMPLES_FULL_SIZE"

    if "print" in sys.argv:
        print(f"\n============================================\nRunning `compiler_cli.py` with args `examples printPretty printSExpr dot`")
        compiler_cli.main("examples printPretty printSExpr dot")
        # runit("python3.6 src/compiler_cli.py examples printPretty printSExpr dot")
    else:
        print(f"\n============================================\nRunning `compiler_cli.py` with args `examples`")
        compiler_cli.main("examples")
        # runit("python3.6 src/compiler_cli.py examples")

    import test_interpreter
    assert test_interpreter.EXAMPLES_FULL_SIZE == len(test_interpreter.EXAMPLES_TO_RUN), "Some entries of test_interpreter.EXAMPLES_TO_RUN are probably commented out"

    import test_typechecker
    test_interpreter.main(sys.argv)
    test_typechecker.main(sys.argv)

    print_all_todos()

# runit("export MYPYPATH=.; mypy --ignore-missing-imports src", "typechecker")
runit("export MYPYPATH=.; mypy src", "typechecker")



