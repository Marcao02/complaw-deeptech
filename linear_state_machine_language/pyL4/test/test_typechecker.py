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
    'serious/SAFE.l4',
]

for filename in EXAMPLES_TO_TYPECHECK:
    print(f"\n---------------------------------\nExample {filename}:")
    in_path = EXAMPLES_SEXPR_ROOT + filename
    parsed = parse_file(in_path)
    prog = L4ContractConstructor(filename).mk_l4contract(parsed)
    typecheck(prog)


def test_eliminate_unbounded_arity():

    eliminate_unbounded_arity({'and*': {3}, '*': {2, 3}, 'max': {2, 3}, 'min': {2},
                               '≤': {2, 3}, '<': {2, 3}, '>': {2, 3},
                               '≥': {2, 3}, '==': {2}, '+': {2, 3}}, fntypes_map)
    print_types_map(fntypes_map)
