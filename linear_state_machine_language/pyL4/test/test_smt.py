import test.test_parser
from src.hard_correctness_checks.normal_forms import eliminate_local_vars
from src.independent.typing_imports import *
from src.hard_correctness_checks.toSMTLIB import SMTLine, SMTCommand, smt_lines_to_str, ToSMTLIB
from src.model.L4Contract import L4Contract

EXAMPLES_TO_USE = [
    'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',
    'serious/SAFE.l4',
    'serious/SAFE_2_liq_eventtypes.l4',
    'from_academic_lit/Farmer_american_call_option_2016.l4',
]

def smt_test(prog:L4Contract, outfilepath:str, verbose=True):
    lines: List[SMTLine] = []

    def heading(s: str, linebreak=True) -> None:
        if linebreak:
            lines.append("")
        lines.append('; ' + s)

    def extend(_lines: Iterable[Union[SMTCommand, str]]):
        lines.extend(_lines)

    eliminate_local_vars(prog)
    toz3 = ToSMTLIB(prog,verbose)
    toz3.prog2smtlibdef()

    heading("Contract param constant declarations")
    extend(toz3.contractParamDecs.values())

    heading("State var constant declarations")
    extend(toz3.stateVarDecs.values())

    heading("Contract param constants extra type info")
    extend(toz3.contractParamExtraTypeAssertions.values())

    heading("State var constants extra type info")
    extend(toz3.stateVarExtraTypeAssertions.values())

    heading("Contract param definitions")
    extend(toz3.contractParamDefns.values())

    heading("Invariants")
    extend(toz3.invariant_assertions)

    for action in prog.actions_iter():
        heading(action.action_id)
        extend(toz3.commands_for_actions[action.action_id])

    with open(outfilepath, 'w') as file:
        file.write(smt_lines_to_str(lines))

# so can run it as a library too, which respects exceptions
def main(examples:Dict[str,L4Contract], verbose=True):
    for examplekey in examples:
        if examplekey in EXAMPLES_TO_USE:
            prog = examples[examplekey]
            outfilepath = "examples/out_smt/" + prog.filename[:-2] + "z3"
            smt_test(prog, outfilepath, verbose=verbose)

def cli(sys_argv:List[str]):
    main(test.test_parser.main(keep=True, verbose=False), verbose=True)

if __name__ == '__main__':
    import sys
    cli(sys.argv)

