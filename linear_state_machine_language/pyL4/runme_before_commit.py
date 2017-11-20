import os, sys


def runit(s, optional_s=""):
    print(f"\n============================================\nRunning {optional_s} `{s}`...")
    os.system(s)

if not "onlytc" in sys.argv and not "tconly" in sys.argv:
    if "print" in sys.argv:
        runit("python3.6 src/cli.py examples printPretty printSExpr dot")
    else:
        runit("python3.6 src/cli.py examples")

    runit("python3.6 src/interpreter.py")

runit("export MYPYPATH=./src; mypy src", "typechecker")