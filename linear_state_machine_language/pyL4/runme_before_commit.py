import os, sys


def runit(s, optional_s=""):
    print(f"\n\nRunning {optional_s} `{s}`...")
    os.system(s)

if "print" in sys.argv:
    runit("python3.6 src/cli.py examples printPretty printSExpr dot")
else:
    runit("python3.6 src/cli.py examples")

runit("python3.6 src/interpreter.py")

runit("mypy src/cli.py", "typechecker")