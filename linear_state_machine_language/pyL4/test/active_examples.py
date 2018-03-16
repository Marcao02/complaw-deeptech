from typing import List, Union, Tuple, Dict

EXAMPLES_SEXPR_ROOT = "./examples/src_sexpr/"
EXAMPLES_UNPARSED_ROOT = "./examples/out_prettyprinted/"

EXAMPLES : List[Union[str,Tuple[str,str,Dict[str,bool]]]] = [
    'toy_and_teaching/test_local_vars.l4',
    'toy_and_teaching/minimal_future-actions.l4',
    'toy_and_teaching/minimal_future-actions2.l4',
    'toy_and_teaching/collatz.l4',
    'toy_and_teaching/collatz2.l4',
    'toy_and_teaching/monster_burger_program_only.l4',
    'toy_and_teaching/partner_assignment_relievable_obligations.l4',
    'toy_and_teaching/partner_assignment_permissions_only.l4',
    'toy_and_teaching/hvitved_modeling_prohibition_trivial_nda.l4',

    'from_academic_lit/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.l4',
    'from_academic_lit/hvitved_master_sales_agreement_full_without_future_obligations.l4',
    'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',
    'from_academic_lit/hvitved_lease.l4',
    'from_academic_lit/hvitved_printer.l4',
    'from_academic_lit/prisacariu_schneider_abdelsadiq_Internet_provision_with_renew.l4',
    'from_academic_lit/Farmer_american_call_option_2016.l4',
    'from_academic_lit/student and grader gothenburg group 2016 A Domain Specific Language for Normative Texts with Timing Constraints.l4',

    # 'serious/SAFE.l4',
    # 'serious/SAFE_2_liq_eventtypes.l4',
    ('serious/SAFE_flags.l4', 'serious/SAFE_mfn.l4', {"HAS_CAP":False, "HAS_DISCOUNT":False}, ),
    ('serious/SAFE_flags.l4', 'serious/SAFE_cap.l4', {"HAS_CAP":True, "HAS_DISCOUNT":False}, ),
    ('serious/SAFE_flags.l4', 'serious/SAFE_discount.l4', {"HAS_CAP":False, "HAS_DISCOUNT":True}, ),
    ('serious/SAFE_flags.l4', 'serious/SAFE_cap_discount.l4', {"HAS_CAP":True, "HAS_DISCOUNT":True}, ),
    'serious/KISS.l4'
]
EXAMPLES_FULL_SIZE = 22
# EXAMPLES_FULL_SIZE = 1