from hard_correctness_checks.normal_forms import eliminate_local_vars
from independent.typing_imports import *
from src.hard_correctness_checks.toZ3 import Z3Line, Z3Statement, z3statements_to_str, ToZ3
from src.model.L4Contract import L4Contract


def smt_test(prog:L4Contract, outfile_name:str):
    lines: List[Z3Line] = []

    def heading(s: str, linebreak=True) -> None:
        if linebreak:
            lines.append("")
        lines.append('; ' + s)

    def extend(_lines: Iterable[Union[Z3Statement, str]]):
        lines.extend(_lines)

    eliminate_local_vars(prog)
    toz3 = ToZ3(prog)
    toz3.prog2z3def()

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
    for inv_assert in toz3.invariant_assertions:
        print(toz3.invariantPrimed(inv_assert[1]))

    for action in prog.actions_iter():
        heading(action.action_id)
        extend(toz3.actionZ3Commands[action.action_id])

    with open(f"{outfile_name}.z3", 'w') as file:
        file.write(z3statements_to_str(lines))
