import test.test_parser
from src.independent.util_for_str import strwbar
from src.hard_correctness_checks.normal_forms import eliminate_local_vars, eliminate_ifthenelse
from src.independent.typing_imports import *
from src.hard_correctness_checks.toSMTLIB import SMTLine, SMTCommand, smt_lines_to_str, ToSMTLIB
from src.model.L4Contract import L4Contract

EXAMPLES_TO_USE = [

    'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',
    'from_academic_lit/Farmer_american_call_option_2016.l4',
    'from_academic_lit/hvitved_lease.l4',
    'from_academic_lit/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.l4',
    # 'from_academic_lit/hvitved_master_sales_agreement_full_without_future_obligations.l4',
    'from_academic_lit/hvitved_printer.l4',
    'from_academic_lit/prisacariu_schneider_abdelsadiq_Internet_provision_with_renew.l4',
    'serious/SAFE.l4',
    'serious/SAFE_2_liq_eventtypes.l4',
    'serious/KISS.l4',
    'toy_and_teaching/minimal_future_actions.l4',
    'toy_and_teaching/minimal_future_actions2.l4',
    'toy_and_teaching/collatz.l4',
    'toy_and_teaching/partner_assignment_relievable_obligations.l4',
    'toy_and_teaching/test_local_vars.l4',
    'toy_and_teaching/hvitved_modeling_prohibition_trivial_nda.l4',
    'toy_and_teaching/monster_burger_program_only.l4'

]

def smt_test(prog:L4Contract, outfilepath:str, verbose=True):
    if verbose:
        print(strwbar(f"\nSMT generation for {prog.filename}",'~'))

    lines: List[SMTLine] = []

    def heading(s: str, linebreak=True) -> None:
        if linebreak:
            lines.append("")
        lines.append('; ' + s)

    def extend(_lines: Iterable[Union[SMTCommand, str]]):
        lines.extend(_lines)

    eliminate_local_vars(prog)
    eliminate_ifthenelse(prog)

    toz3 = ToSMTLIB(prog,verbose)
    toz3.prog2smtlibdef()

    heading("Boilerplate")
    extend(toz3.boilerplate_smtlib())

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

    for sit in prog.situations_iter():
        # heading(sit.situation_id)
        toz3.situation2smtlib(sit)

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

