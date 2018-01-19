from compiler.parse_sexpr import parse_file
from compiler.sexpr_to_L4Contract import L4ContractConstructor
from typesystem.reducers import eliminate_unbounded_arity, print_types_map
from typesystem.standard_types import fntypes_map
from typesystem.typecheck import typecheck

EXAMPLES_SEXPR_ROOT = "./examples_sexpr/"
EXAMPLES_TO_TYPECHECK = [
    'toy_and_teaching/test_local_vars.l4',
    'toy_and_teaching/partner_assignment_relievable_obligations.l4',
    'toy_and_teaching/collatz.l4',
    'toy_and_teaching/monster_burger_program_only.l4',

    'from_academic_lit/hvitved_lease.l4',
    'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',
    'from_academic_lit/hvitved_master_sales_agreement_full_without_future_obligations.l4',
    # 'from_academic_lit/hvitved_printer.l4',

    'serious/SAFE.l4',
]

for filename in EXAMPLES_TO_TYPECHECK:
    msg = f"Example {filename}:"
    print(f"\n{'-'*len(msg)}\n{msg}")
    in_path = EXAMPLES_SEXPR_ROOT + filename
    parsed = parse_file(in_path)
    prog = L4ContractConstructor(filename).mk_l4contract(parsed)
    typecheck(prog)
    print(f"{sum((len(oft.illtyped_memo) + len(oft.range_memo) for oft in prog.overloaded_fntypes()))} cache entries")


def test_eliminate_unbounded_arity():

    eliminate_unbounded_arity({'and*': {3}, '*': {2, 3}, 'max': {2, 3}, 'min': {2},
                               '≤': {2, 3}, '<': {2, 3}, '>': {2, 3},
                               '≥': {2, 3}, '==': {2}, '+': {2, 3}}, fntypes_map)
    print_types_map(fntypes_map)
