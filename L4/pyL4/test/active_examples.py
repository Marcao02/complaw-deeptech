from typing import List, Union, Tuple, Dict

EXAMPLES_SEXPR_ROOT = "../examples/src_sexpr/"
EXAMPLES_UNPARSED_ROOT = "../examples/out_reduced/"
EXAMPLES_HTML_ROOT = "../examples/out_html/"

EXAMPLES : List[Union[str,Tuple[str,str,Dict[str,bool],Dict[str,bool]]]] = [
    'test/test_symbolic_exec_time.l4',
    'test/test_symbolic_exec_ifelse_halting_split.l4',
    'test/test_symbolic_exec_ifelse_halting.l4',
    'test/test_symbolic_exec_halting.l4',
    'test/test_symbolic_exec_halting_easiest.l4',
    'test/test_symbolic_exec_halting_harder.l4',
    'test/test_symbexec_multiwrite.l4',
    'test/test_symbexec_multiwrite_error.l4',
    'test/test_local_vars.l4',


    'toy_and_teaching/minimal_future-actions.l4',
    'toy_and_teaching/minimal_future-actions2.l4',
    'toy_and_teaching/collatz.l4',
    'toy_and_teaching/collatz2.l4',
    'toy_and_teaching/monster_burger_program_only.l4',
    'toy_and_teaching/partner_assignment_permissions_only.l4',
    'toy_and_teaching/hvitved_modeling_prohibition_trivial_nda.l4',
    'toy_and_teaching/tutorialSAFE.l4',

    'from_academic_lit/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.l4',
    'from_academic_lit/hvitved_master_sales_agreement_full_without_future_obligations.l4',
    'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',
    'from_academic_lit/hvitved_lease.l4',
    'from_academic_lit/hvitved_printer.l4',
    'from_academic_lit/prisacariu_schneider_abdelsadiq_Internet_provision_with_renew.l4',
    'from_academic_lit/Farmer_american_call_option_2016.l4',
    'from_academic_lit/student and grader gothenburg group 2016 A Domain Specific Language for Normative Texts with Timing Constraints.l4',

    ('from_academic_lit/goodenough_flood_loan_verbatim.l4',
     'from_academic_lit/goodenough_flood_loan_verbatim_noBreach.l4',
     None, {'~must':'may'} ),

    ('from_academic_lit/goodenough_flood_loan_verbatim.l4',
     'from_academic_lit/goodenough_flood_loan_verbatim_withBreach.l4',
     None, {'~must':'must'} ),

    'serious/KISS.l4',
    ('serious/SAFE.l4', 'serious/SAFE_mfn.l4', {"HAS_CAP":False, "HAS_DISCOUNT":False}, None),
    ('serious/SAFE.l4', 'serious/SAFE_cap.l4', {"HAS_CAP":True, "HAS_DISCOUNT":False}, None),
    ('serious/SAFE.l4', 'serious/SAFE_discount.l4', {"HAS_CAP":False, "HAS_DISCOUNT":True}, None),
    ('serious/SAFE.l4', 'serious/SAFE_cap_discount.l4', {"HAS_CAP":True, "HAS_DISCOUNT":True}, None),
    # ('serious/wip/SAFE-nlg/SAFE_nlg_compatible.l4', 'serious/wip/SAFE-nlg/SAFE_nlg_compatible_cap_discount.l4', {"HAS_CAP":True, "HAS_DISCOUNT":True}, None),


]

ALL_BEFORE_EXPAND_EXAMPLE_KEYS : List[str] = []
ALL_AFTER_EXPAND_EXAMPLE_KEYS : List[str] = []
for x in EXAMPLES:
    if isinstance(x,str):
        ALL_BEFORE_EXPAND_EXAMPLE_KEYS.append(x)
        ALL_AFTER_EXPAND_EXAMPLE_KEYS.append(x)
    else:
        ALL_BEFORE_EXPAND_EXAMPLE_KEYS.append(x[0])
        ALL_AFTER_EXPAND_EXAMPLE_KEYS.append(x[1])

EXAMPLES_FULL_SIZE = 32
# EXAMPLES_FULL_SIZE = 1