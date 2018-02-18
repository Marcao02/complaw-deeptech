from typing import List, Dict

from hard_correctness_checks.normal_forms import eliminate_local_vars, eliminate_ifthenelse
from src.model.L4Contract import L4Contract
from src.parse_to_model.sexpr_to_L4Contract import L4ContractConstructor
from src.correctness_checks import test_fns
from src.independent.parse_sexpr import parse_file, prettySExprStr

from test.active_examples import *

# filename = filesubpath.split("/")[-1].split(".")[0]

# so can run it as a library too, which respects exceptions
def main(keep=False, verbose=False) -> Dict[str,L4Contract]:
    rv : Dict[str,L4Contract] = {}
    for filesubpath in EXAMPLES:
        # print(f"\n---------------------------------\nExample {filesubpath}:")
        in_path = EXAMPLES_SEXPR_ROOT + filesubpath
        if verbose:
            print("\nLooking at file " + filesubpath + ":\n")
        parsed = parse_file(in_path)
        if verbose:
            print(prettySExprStr(parsed))
        assembler = L4ContractConstructor(filesubpath, verbose)
        prog = assembler.mk_l4contract(parsed)
        # eliminate_local_vars(prog)
        # eliminate_ifthenelse(prog)
        print(prog)

        if keep:
            rv[filesubpath] = prog

        if verbose:
            print("\n~~~~~~~~~~~~~~\nRunning correctness checks...")
        for check in test_fns:
            check(assembler)
            # print()
            # if check(assembler):
            #     print( check.__name__ + " ok" )
    return rv

def cli(sys_argv:List[str]):
    main(False,True)

if __name__ == '__main__':
    import sys
    cli(sys.argv)

