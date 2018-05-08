from datetime import timedelta
from itertools import chain
from typing import Sequence, Tuple, Optional

from src.hard_correctness_checks.normal_forms import eliminate_ifthenelse, eliminate_local_vars
from src.constants_and_defined_types import *
from src.interpreter.interpreter_runner import evalTrace
from src.model.EventsAndTraces import CompleteTrace, Trace, Event, breachSituationId, EventType, breachActionId
from src.model.L4Contract import L4Contract
from test import test_parser
from test.active_examples import EXAMPLES, ALL_AFTER_EXPAND_EXAMPLE_KEYS
from test.test_SAFE import all_safe_tests
from test.test_util import *


traces_toy_and_teaching : Sequence[ Tuple[str, Union[Trace,CompleteTrace]] ] = (
    ('test/test_symbexec_multiwrite.l4',  CompleteTrace(
        {'N':1200},
        (
            event('DoIt', 'Env', 0),
            # event('FinishIt', 'Env', 0, {'z':81})
        ),
        FULFILLED_SITUATION_LABEL,
        {"v": 2400})
     ),

    ('test/test_symbexec_multiwrite.l4',  CompleteTrace(
        {'N':1050},
        (
            event('DoIt', 'Env', 0),
            # event('FinishIt', 'Env', 0, {'z':81})
        ),
        FULFILLED_SITUATION_LABEL,
        {"v": 0})
     ),
    ('test/test_symbexec_multiwrite.l4',  CompleteTrace(
        {'N':999},
        ( event('DoIt', 'Env', 0), ),
        FULFILLED_SITUATION_LABEL,
        {"v": 999})
     ),


    ('test/test_symbexec_multiwrite_error.l4',  CompleteTrace(
        {'N':950},
        (
            event('DoIt', 'Env', 0),
            # event('FinishIt', 'Env', 0, {'z':81})
        ),
        FULFILLED_SITUATION_LABEL,
        {"v": 1900})
     ),

    ('test/test_symbexec_multiwrite_error.l4',  CompleteTrace(
        {'N':1050},
        (
            event('DoIt', 'Env', 0),
            # event('FinishIt', 'Env', 0, {'z':81})
        ),
        FULFILLED_SITUATION_LABEL,
        {"v": 2100})
     ),


    ('test/test_local_vars.l4', CompleteTrace(
        {},
        (
            event('DoIt', 'Env', 0),
            event('FinishIt', 'Env', 0, {'z':81})
        ),
        FULFILLED_SITUATION_LABEL,
        {"n": 81})
     ),

    ('toy_and_teaching/minimal_future-actions.l4', CompleteTrace(
        {},
        (event('Throw', 'I'),
         event('Throw', 'I'),
         event('Throw', 'I'),
         foevent('Catch', 'I', 0, {'m': 3}),
         foevent('Catch', 'I', 0, {'m': 2}),
         foevent('Catch', 'I', 0, {'m': 1}),
         event('EnterFulfilled', 'Env', 0), # not finished. see next test.
         ), breachSituationId('Env'))
    ),

    ('toy_and_teaching/minimal_future-actions.l4', CompleteTrace(
        {},
        (event('Throw', 'I'),  # n = 1 after
         event('Throw', 'I'),  # n = 2 after
         event('Stand', 'I'),  # n = 3 after
         event('Throw', 'I'),  # n = 4 after
         foevent('Catch', 'I', 0, {'m': 1}),
         foevent('Catch', 'I', 0, {'m': 4}),
         foevent('Catch', 'I', 0, {'m': 2}),
         event('Stand', 'I'),  # n = 5 after
         event('EnterFulfilled', 'Env', 0),
         ), FULFILLED_SITUATION_LABEL)
     ),

    ('toy_and_teaching/minimal_future-actions2.l4', CompleteTrace(
        {}, (
        event('Throw', 'I', 0, {'n':4}),
        event('Throw', 'I', 0, {'n':3}),
        event('Throw', 'I', 0, {'n':1}),
        event('Throw', 'I', 0, {'n':2}),
        foevent('Catch', 'I', 0, {'n': 1}),
        foevent('Catch', 'I', 0, {'n': 3}),
        foevent('Catch', 'I', 0, {'n': 4}),
        foevent('Catch', 'I', 0, {'n': 2}),
        event('EnterFulfilled', 'I', 0),
         ), FULFILLED_SITUATION_LABEL)
     ),

    ('toy_and_teaching/minimal_future-actions.l4', CompleteTrace(
        {},
        (   event('Throw','I'),
            event('Throw','I'),
            event('Stand','I'),
            event('Throw','I'),
            foevent('Catch', 'I', 0, {'m': 1}),
            foevent('Catch', 'I', 0, {'m': 4}),
            foevent('Catch', 'I', 0, {'m': 3}),
            # event('EnterFulfilled', 'Env', 0),
        ), breachSituationId('I'))
    ),


    ('toy_and_teaching/collatz.l4', CompleteTrace(
        {'START': 12},
        (event('DivideBy2'),
         event('DivideBy2'),
         event('TripplePlus1'),
         event('DivideBy2'),
         event('TripplePlus1'),
         event('DivideBy2'),
         event('DivideBy2'),
         event('DivideBy2'),
         event('DivideBy2'),
         event('EnterFulfilled'),
         ), FULFILLED_SITUATION_LABEL)
     ),

    ('toy_and_teaching/collatz2.l4', CompleteTrace(
        {'START': 12}, (
         event('NextInSeq'),
         event('NextInSeq'),
         event('NextInSeq'),
         event('NextInSeq'),
         event('NextInSeq'),
         event('NextInSeq'),
         event('NextInSeq'),
         event('NextInSeq'),
         event('NextInSeq')
         ), 'AfterNextInSeq', {'x':1})
     ),

    ('toy_and_teaching/monster_burger_program_only.l4', CompleteTrace({},(
        # start situation implicit
        event('RequestCookMB', 'Challenger',1),
        event('ServeMB', 'Restaurant', 2),
        # nextTSEvent('AnnounceMBFinished', 'Challenger'),
        event('AnnounceMBFinished', 'Challenger', 8),
        event('CheckCompletionClaim', 'Restaurant', 9),
        event('RejectCompletionClaim', 'Restaurant', 9),
        event('EnterEatingMB', 'Env', 9),
        event('AnnounceMBFinished','Challenger',15),
        event('CheckCompletionClaim', 'Restaurant',15),
        event('VerifyCompletionClaim', 'Restaurant',15)
        ), FULFILLED_SITUATION_LABEL)
     ),
)
traces_from_academic_lit: Sequence[Tuple[str, Union[Trace, CompleteTrace]]] = (
    ('from_academic_lit/prisacariu_schneider_abdelsadiq_Internet_provision_with_renew.l4',
     CompleteTrace({},(
            event('RaiseTraffic','Client',0),
            event('LowerTraffic','Client',20),
            event('Pay','Client',21, {'x':1}),
            event('SendCancelNoticeByPost','ISP',50),
            event('SendCancelNoticeByEmail','ISP',50),
            event('EnterFulfilled','Env',50)
        ),
        FULFILLED_SITUATION_LABEL
    )),
    ('from_academic_lit/prisacariu_schneider_abdelsadiq_Internet_provision_with_renew.l4',
     CompleteTrace({},(
            event('RaiseTraffic','Client',0),
            event('SendDelayEmail','Client',20),
            event('LowerTraffic','Client',24),
            event('Pay','Client',48, {'x':2}),
            event('SendCancelNoticeByPost','ISP',50),
            event('SendCancelNoticeByEmail','ISP',50),
            event('EnterFulfilled','Env',50)
        ),
        FULFILLED_SITUATION_LABEL
    )),

    ('from_academic_lit/hvitved_lease.l4',
     CompleteTrace({}, (
        event('EnsureApartmentReady', 'Landlord', 0), # 1
        event('StartLeaseTerm', ENV_ROLE, 0), # 2
        event('PayRent', 'Tenant', 7), # 3
        event('EnterMonthEnded', ENV_ROLE, 30), # Jan 31
        event('EnterMonthStarted', ENV_ROLE, 31),
        event('RequestTerminationFromMonthStarted', 'Tenant', 34),
        event('PayRent', 'Tenant', 35), # 35
        event('EnterMonthEnded', ENV_ROLE, 59),  # Feb 29
        event('EnterMonthStarted', ENV_ROLE, 60), # Mar 1
        event('PayRent', 'Tenant', 65),
        event('EnterMonthEnded', ENV_ROLE, 90),  # Mar 31
        event('EnterMonthStarted', ENV_ROLE, 91), # April 1
        event('PayRent', 'Tenant', 93),
        event('EnterMonthEnded', ENV_ROLE, 120),  # April 30
        event('EnterLeaseTermEnded', ENV_ROLE, 120),
        event('MoveOut', 'Tenant', 120),
      ), FULFILLED_SITUATION_LABEL)
    ),
    ( 'from_academic_lit/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.l4', CompleteTrace(
        {
            'MAX_UNITS' : 1000,
            'CONTRACT_LIFE' : timedelta(days=365),
            'PRICE_PER_UNIT' : 100
        },(
        # start situation implicit
        event('SubmitNewOrder', 'Customer', 5, {'quantity':300}), # orderid 0
        event('SubmitNewOrder', 'Customer', 6, {'quantity': 200}),  # orderid 1
        foevent('Deliver', 'Vendor', 10, {'quantity':200,'orderid':1}),
        foevent('Deliver', 'Vendor', 19, {'quantity':300,'orderid':0}),
        fpevent('EmailInvoice', 'Vendor', 40, {'quantity':200,'orderid':1}),
        event('SubmitNewOrder', 'Customer', 41, {'quantity': 500}),  # orderid 2
        foevent('Deliver', 'Vendor', 42, {'quantity':500,'orderid':2}),
        event('EnterFulfilled', 'Env', 50)
        ), breachSituationId('Customer'))
        # It should end in a breach by Customer due to the unpaid invoice.
    ),

    ( 'from_academic_lit/hvitved_master_sales_agreement_full_without_future_obligations.l4', CompleteTrace(
        {
            'MAX_UNITS' : 1000,
            'CONTRACT_LIFE' : timedelta(days=365),
            'PRICE_PER_UNIT' : 100
        },(
        # start situation implicit
        event('SubmitNewOrder', 'Customer', 5, {'quantity':300}), # orderid 0
        event('SubmitNewOrder', 'Customer', 6, {'quantity': 200}),  # orderid 1
        event('Deliver', 'Vendor', 10, {'order':(200,1)}),
        event('Deliver', 'Vendor', 19, {'order':(300,0)}),
        event('EmailInvoice', 'Vendor', 40, {'order':(200,1)}),
        event('SubmitNewOrder', 'Customer', 41, {'quantity': 500}),  # orderid 2
        event('Deliver', 'Vendor', 42, {'order':(500,2)}),
        event(breachActionId('Customer'), 'Env', 62)
        ), breachSituationId('Customer'))

    ),

    # ( 'from_academic_lit/hvitved_master_sales_agreement_full_without_future_obligations.l4', CompleteTrace(
    #     {
    #         'MAX_UNITS' : 1000,
    #         'CONTRACT_LIFE' : timedelta(days=365),
    #         'PRICE_PER_UNIT' : 100
    #     },(
    #     # start situation implicit
    #     event('SubmitNewOrder', 'Customer', 5, {'quantity':300}), # orderid 0
    #     event('SubmitNewOrder', 'Customer', 6, {'quantity': 200}),  # orderid 1
    #     event('Deliver', 'Vendor', 10, {'order':(200,1)}),
    #     event('Deliver', 'Vendor', 19, {'order':(300,0)}),
    #     event('EmailInvoice', 'Vendor', 40, {'order':(200,1)}),
    #     event('SubmitNewOrder', 'Customer', 41, {'quantity': 500}),  # orderid 2
    #     event('Deliver', 'Vendor', 42, {'order':(500,2)}),
    #     event('EnterFulfilled', 'Env', 50)
    #     ), breachSituationId('Customer'))
    #     # It should end in a breach by Customer due to the unpaid invoice.
    # ),

    ('from_academic_lit/hvitved_instalment_sale--simplified_time.l4', CompleteTrace({},(
        # start situation implicit
        event('PayInstallment', 'Buyer', 30, {'amount':500}),
        event('PayInstallment', 'Buyer', 60, {'amount':500}),
        event('PayInstallment', 'Buyer', 90, {'amount':8000}),
        event('PayLastInstallment', 'Buyer', 120, {'amount':3074}),
        ), FULFILLED_SITUATION_LABEL)
    ),

    ('from_academic_lit/hvitved_instalment_sale--simplified_time.l4', CompleteTrace({},(
        event('PayInstallment', 'Buyer', 30, {'amount':499}),
        ), breachSituationId('Buyer'))
    ),

    ('from_academic_lit/hvitved_instalment_sale--simplified_time.l4', CompleteTrace({},(
        event('PayInstallment', 'Buyer', 30, {'amount':500}),
        event('PayInstallment', 'Buyer', 60, {'amount':500}),
        event('PayInstallment', 'Buyer', 90, {'amount':7999}),
        event('PayLastInstallment', 'Buyer', 120, {'amount':1000}),
        ), breachSituationId('Buyer'))
    ),

    ('from_academic_lit/hvitved_instalment_sale--simplified_time.l4', CompleteTrace({},(
        event('PayInstallment', 'Buyer', 30, {'amount':500}),
        event('PayInstallment', 'Buyer', 60, {'amount':500}),
        event('PayInstallment', 'Buyer', 90, {'amount':8500}),
        event('PayLastInstallment', 'Buyer', 120, {'amount':2524}),
        ), FULFILLED_SITUATION_LABEL)
    ),

)


traces_serious: Sequence[ Union[ Tuple[str, Union[Trace,CompleteTrace]], Tuple[str, Union[Trace,CompleteTrace], str]] ] = list(all_safe_tests)

# this is used in runme_before_commit.py b/c I often comment out some of the entries of EXAMPLES_TO_RUN
EXAMPLES_FULL_SIZE = sum((len({x[0] for x in col}) for col in [traces_toy_and_teaching, traces_from_academic_lit, traces_serious]))
traces = chain(traces_toy_and_teaching, traces_from_academic_lit, traces_serious)

EXAMPLES_TO_RUN = [
        'from_academic_lit/hvitved_master_sales_agreement_full_without_future_obligations.l4',

        'from_academic_lit/prisacariu_schneider_abdelsadiq_Internet_provision_with_renew.l4',
        'from_academic_lit/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.l4',
        'from_academic_lit/hvitved_lease.l4',

        'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',

        'toy_and_teaching/minimal_future-actions.l4',
        'toy_and_teaching/minimal_future-actions2.l4',
        'toy_and_teaching/collatz.l4',
        'toy_and_teaching/collatz2.l4',
        'toy_and_teaching/monster_burger_program_only.l4',

        'test/test_local_vars.l4',
        'test/test_symbexec_multiwrite.l4',
        'test/test_symbexec_multiwrite_error.l4',

        # 'serious/SAFE.l4',
        'serious/SAFE_cap.l4',
        'serious/SAFE_discount.l4',
        'serious/SAFE_cap_discount.l4',
        'serious/wip/SAFE-nlg/SAFE_nlg_compatible_cap_discount.l4',
        # 'serious/SAFE_mfn.l4',
        # 'serious/SAFE_2_liq_eventtypes.l4'
    ]

# so can run it as a library too, which respects exceptions
def main(examples:Dict[str,L4Contract], verbose=True):
    for trace in traces:
        examplekey = trace[0]
        if examplekey not in ALL_AFTER_EXPAND_EXAMPLE_KEYS:
            raise Exception("probably have typo in this path: ", examplekey)
        if examplekey in examples and examplekey in EXAMPLES_TO_RUN:
            if verbose:
                print("\nRunning test trace for " + examplekey)
            try:
                prog = examples[examplekey]
                evalTrace(trace[1], prog, verbose=verbose, debug=False)
                eliminate_local_vars(prog)
                evalTrace(trace[1], prog, verbose=verbose, debug=False)
                eliminate_ifthenelse(prog)
                evalTrace(trace[1], prog, verbose=verbose, debug=False)
            except Exception as e:

                print(f"Problem with {examplekey} test case")
                if len(trace) == 3:
                    print(trace[2])
                raise e


def cli(sys_argv:Sequence[str]):
    main(test_parser.main(keep=True, verbose=False), verbose=True)

if __name__ == '__main__':
    import sys
    cli(sys.argv)


# 'examples_sexpr/hvitved_lease.LSM': [
#     nextTSEvent('Move_In'),
#     nextTSEvent('Lease_Term_Started'),
#     nextTSEvent('EnsureApartmentReady'),
#     nextTSEvent('Month_Started'),
#     nextTSEvent('PayRent'),
#     nextTSEvent('Month_Ended'),
#     nextTSEvent('Month_Started'),
#     nextTSEvent('RentDue'),
#     nextTSEvent('PayRent'),
#     nextTSEvent('Month_Ended'),
#     nextTSEvent('Month_Started'),
#     nextTSEvent('Request_Termination_At_Rent_Or_Before'),
#     nextTSEvent('PayRent'),
#     nextTSEvent('Month_Ended'),
#     nextTSEvent('Month_Started'),
#     nextTSEvent('PayRent'),
#     nextTSEvent('Month_Ended'),
#     nextTSEvent('Month_Started'),
#     nextTSEvent('PayRent'),
#     nextTSEvent('Request_Termination_After_Rent'),
#     nextTSEvent('Month_Ended'),
#     nextTSEvent('Lease_Term_Ended'),
#     nextTSEvent('Move_Out'),
#     nextTSEvent('Month_Started'),
# ],