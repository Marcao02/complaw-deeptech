import test.test_parser
from src.hard_correctness_checks.symbolic_execution.symbolic_execution import symbolic_execution
from src.hard_correctness_checks.normal_forms import eliminate_local_vars, eliminate_ifthenelse
from src.independent.typing_imports import *
from src.model.L4Contract import L4Contract



# so can run it as a library too, which respects exceptions
def main(examples: Dict[str, L4Contract], verbose=True):

    se_tests = map(lambda x: examples[x], [
        'serious/SAFE_mfn.l4',
        'serious/SAFE_cap.l4',
        'serious/SAFE_discount.l4',
        'serious/SAFE_cap_discount.l4',
        'test/test_local_vars.l4',
        'test/test_symbolic_exec_halting_easiest.l4',
        'test/test_symbolic_exec_halting.l4',
        'test/test_symbolic_exec_ifelse_halting_split.l4',
        'test/test_symbolic_exec_ifelse_halting.l4',
        # 'test/test_symbolic_exec_halting_harder.l4', # uses `even`
        'test/test_symbolic_exec_time.l4',
        'toy_and_teaching/monster_burger_program_only.l4',
        'toy_and_teaching/meng_buy_booze.l4',
        # 'from_academic_lit/hvitved_lease.l4', # uses `monthStartDay_td`
        'toy_and_teaching/hvitved_modeling_prohibition_trivial_nda.l4',
        # 'from_academic_lit/hvitved_master_sales_agreement_full_without_future_obligations.l4', # uses currently-unsupported data structure
        # 'from_academic_lit/hvitved_instalment_sale--simplified_time.l4' # goes forever because of 0 duration actions
        'from_academic_lit/Farmer_american_call_option_2016.l4',  # need to implement next_event_dt first
        # 'toy_and_teaching/partner_assignment_permissions_only.l4', # infinite...
        'test/test_symbexec_multiwrite.l4',

        # currently failing!
        # 'test/test_symbexec_multiwrite_error.l4',

        # 'from_academic_lit/wip/goodenough_flood_loan_verbatim_happypath.l4'
        # 'from_academic_lit/goodenough_flood_loan_verbatim_noBreach.l4',


        'from_academic_lit/hvitved_printer.l4',

    ])
    for prog in se_tests:
        print("\n\n\nsymbolic exec for " + prog.filename)
        eliminate_ifthenelse(prog)
        eliminate_local_vars(prog)
        if prog.filename == "test/test_symbexec_multiwrite_error.l4":
            try:
                symbolic_execution(prog)
                raise Exception("Expected exception not thrown")
            except AssertionError as e:
                assert e.args[0].startswith("Variable"), "Exception test case failed."
                print("Test case ended in exception, as expected")
        else:
            symbolic_execution(prog)


def cli(sys_argv: List[str]):
    main(test.test_parser.main(keep=True, verbose=False), verbose=True)


if __name__ == '__main__':
    import sys

    cli(sys.argv)

