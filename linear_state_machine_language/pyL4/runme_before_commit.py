import os


os.system("python3.6 src/cli.py examples printPretty printSExpr dot")

print("\n\nRunning typechecker...")
os.system("mypy src/cli.py")