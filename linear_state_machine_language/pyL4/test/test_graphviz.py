from src.independent.typing_imports import *
from src.model.L4Contract import L4Contract
from src.state_diagram_generation import contractToDotFile



def main(examples:Dict[str,L4Contract]):
    for examplekey in examples:
        prog = examples[examplekey]
        contractToDotFile(prog, "examples/out_graphviz", True, True)

def cli(sys_argv:List[str]):
    raise NotImplementedError

if __name__ == '__main__':
    import sys
    cli(sys.argv)