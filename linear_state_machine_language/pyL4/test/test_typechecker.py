from typing import Sequence

from src.independent.parse_sexpr import parse_file
from src.compiler.compiler_cli import EXAMPLES_SEXPR_ROOT
from src.compiler.sexpr_to_L4Contract import L4ContractConstructor

from src.typesystem.standard_function_types import STANDARD_FNTYPES, print_types_map
from src.typesystem.typecheck import typecheck_prog
from src.temp_src.for_safe import doit_for_safe
from typesystem.standard_subtype_graph import STANDARD_SUBSORTING_GRAPH

EXAMPLES_TO_TYPECHECK = [
    'toy_and_teaching/test_local_vars.l4',
    'toy_and_teaching/partner_assignment_relievable_obligations.l4',
    'toy_and_teaching/collatz.l4',
    'toy_and_teaching/monster_burger_program_only.l4',
    'toy_and_teaching/minimal_future-actions.l4',
    'toy_and_teaching/minimal_future-actions2.l4',

    'from_academic_lit/hvitved_lease.l4',
    'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',
    'from_academic_lit/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.l4',
    'from_academic_lit/hvitved_master_sales_agreement_full_without_future_obligations.l4',
    'from_academic_lit/hvitved_printer.l4',
    'from_academic_lit/prisacariu_schneider_abdelsadiq_Internet_provision_with_renew.l4',

    'serious/SAFE.l4',
    'serious/SAFE_2_liq_eventtypes.l4',
    'serious/KISS.l4',
]

def test_eliminate_unbounded_arity():

    eliminate_unbounded_arity({'and*': {3}, '*': {2, 3}, 'max': {2, 3}, 'min': {2},
                               '≤': {2, 3}, '<': {2, 3}, '>': {2, 3},
                               '≥': {2, 3}, '==': {2}, '+': {2, 3}}, STANDARD_FNTYPES)
    print_types_map(STANDARD_FNTYPES)


def main(sys_argv:Sequence[str]):
    print_types_map(STANDARD_FNTYPES)

    # return
    for filename in EXAMPLES_TO_TYPECHECK:
        msg = f"Example {filename}:"
        # print(f"\n{'='*len(msg)}\n{msg}")
        print(f"\n{msg}\n{'='*len(msg)}")
        in_path = EXAMPLES_SEXPR_ROOT + filename
        parsed = parse_file(in_path)
        prog = L4ContractConstructor(filename).mk_l4contract(parsed)
        typecheck_prog(prog)
        # print(f"{sum((len(oft.illtyped_memo) + len(oft.range_memo) for oft in prog.overloaded_fntypes()))} cache entries")

if __name__ == '__main__':
    import sys
    main(sys.argv)