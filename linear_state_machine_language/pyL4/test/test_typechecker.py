from typing import Sequence, Dict

import test_parser
from model.L4Contract import L4Contract
from test_parser import EXAMPLES_SEXPR_ROOT
from src.independent.parse_sexpr import parse_file
from src.parse_to_model.sexpr_to_L4Contract import L4ContractConstructor
from src.typechecking.standard_function_types import STANDARD_FNTYPES, print_types_map
from src.typechecking.typecheck import typecheck_prog

EXAMPLES_TO_TYPECHECK = [
    'toy_and_teaching/test_local_vars.l4',
    'toy_and_teaching/partner_assignment_relievable_obligations.l4',
    'toy_and_teaching/collatz.l4',
    'toy_and_teaching/monster_burger_program_only.l4',
    'toy_and_teaching/minimal_future-actions.l4',
    'toy_and_teaching/minimal_future-actions2.l4',
    'toy_and_teaching/hvitved_modeling_prohibition_trivial_nda.l4',

    'from_academic_lit/hvitved_lease.l4',
    'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',
    'from_academic_lit/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.l4',
    'from_academic_lit/hvitved_master_sales_agreement_full_without_future_obligations.l4',
    'from_academic_lit/hvitved_printer.l4',
    'from_academic_lit/prisacariu_schneider_abdelsadiq_Internet_provision_with_renew.l4',
    'from_academic_lit/Farmer_american_call_option_2016.l4',

    'serious/SAFE.l4',
    'serious/SAFE_2_liq_eventtypes.l4',
    'serious/KISS.l4',
]

def main(examples:Dict[str,L4Contract], verbose=True):
    # print_types_map(STANDARD_FNTYPES)
    for examplekey in examples:
        if examplekey in EXAMPLES_TO_TYPECHECK:
            # msg = f"Example {filename}:"
            # print(f"\n{'='*len(msg)}\n{msg}")
            # print(f"\n{msg}\n{'='*len(msg)}")
            # in_path = EXAMPLES_SEXPR_ROOT + filename
            # parsed = parse_file(in_path)
            # prog = L4ContractConstructor(filename).mk_l4contract(parsed)
            prog = examples[examplekey]
            typecheck_prog(prog,verbose=verbose)
            # print(f"{sum((len(oft.illtyped_memo) + len(oft.range_memo) for oft in prog.overloaded_fntypes()))} cache entries")

def cli(sys_argv:Sequence[str]):
    main(test_parser.main(keep=True,verbose=False), verbose=False)

if __name__ == '__main__':
    import sys
    cli(sys.argv)