import os, sys


def runit(s, optional_s=""):
    line2 = f"Running {optional_s} `{s}`"
    print(f"\n{'='*len(line2)}\n{line2}")
    os.system(s)

if not "onlytc" in sys.argv and not "tconly" in sys.argv:
    import cli
    if "print" in sys.argv:
        print(f"\n============================================\nRunning `cli` with args `examples printPretty printSExpr dot`")
        cli.main("examples printPretty printSExpr dot")
        # runit("python3.6 src/cli.py examples printPretty printSExpr dot")
    else:
        print(f"\n============================================\nRunning `cli` with args `examples`")
        cli.main("examples")
        # runit("python3.6 src/cli.py examples")

    import test_interpreter
    test_interpreter.main(sys.argv)
    # runit("python3.6 src/interpreter.py")

runit("export MYPYPATH=.; mypy src", "typechecker")
