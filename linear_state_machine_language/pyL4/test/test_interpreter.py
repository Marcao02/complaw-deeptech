from datetime import timedelta
from itertools import chain
from math import inf
from typing import Sequence, Tuple, Optional, Iterable

import test_parser
from hard_correctness_checks.normal_forms import eliminate_ifthenelse, eliminate_local_vars
from src.constants_and_defined_types import *
from src.independent.parse_sexpr import prettySExprStr, parse_file
from src.independent.util import castid
from src.interpreter.interpreter_runner import evalTrace
from src.model.EventsAndTraces import CompleteTrace, Trace, Event, breachSituationId, EventType, breachActionId
from src.model.L4Contract import L4Contract
from src.parse_to_model.sexpr_to_L4Contract import L4ContractConstructor


def event(action_id:str, role_id:str = ENV_ROLE,
          timestamp: int = 0,
          params:Optional[Dict[str, Data]] = None,
          eventType: Optional[EventType] = None ) -> Event:
    if eventType is None:
        eventType = EventType.env_next if role_id == ENV_ROLE else EventType.party_next
    params = params or dict()
    return Event(action_id=castid(ActionId, action_id), role_id=castid(RoleId, role_id),
                 timestamp= timestamp,
                 params_by_abap_name=cast(ABAPNamedSubst, params) if params else None,
                 params=list(params.values()) if params else None,
                 type=eventType)

def foevent(action_id:str, role_id:str = ENV_ROLE,
          timestamp: int = 0, params:Optional[Dict[str, Data]] = None) -> Event:
    return event(action_id, role_id, timestamp , params, EventType.fulfill_floating_obligation)

def fpevent(action_id:str, role_id:str = ENV_ROLE,
          timestamp:int = 0, params:Optional[Dict[str, Data]] = None) -> Event:
    return event(action_id,role_id,timestamp,params,EventType.use_floating_permission)

inc_timestamp = 0

def firstTSEvent(action_id:str,
                 role_id:str = ENV_ROLE,
                 params:Optional[Dict[str, Data]] = None,
                 eventType: Optional[EventType] = None) -> Event:
    global inc_timestamp
    inc_timestamp = 0
    return nextTSEvent(action_id, role_id, params, eventType)

def nextTSEvent(action_id:str,
                role_id:str = ENV_ROLE,
                params:Optional[Dict[str, Data]] = None,
                eventType: Optional[EventType] = None) -> Event:
    global inc_timestamp
    inc_timestamp = inc_timestamp + 1
    newevent = event(action_id, role_id, inc_timestamp, params, eventType)
    return newevent

def sameTSEvent(action_id:str,
                role_id:str = ENV_ROLE,
                params:Optional[Dict[str, Data]] = None,
                eventType: Optional[EventType] = None) -> Event:
    global inc_timestamp
    return event(action_id, role_id, inc_timestamp, params, eventType)



K = 1000
M = 1000000

traces_toy_and_teaching : Sequence[ Tuple[str, Union[Trace,CompleteTrace]] ] = (
    ('toy_and_teaching/test_local_vars.l4', CompleteTrace(
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

    ('from_academic_lit//hvitved_lease.l4',
     CompleteTrace({}, (
        firstTSEvent('EnsureApartmentReady', 'Landlord'), # 1
        nextTSEvent('StartLeaseTerm'), # 2
        nextTSEvent('PayRent', 'Tenant'), # 3
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


traces_serious: Sequence[ Union[ Tuple[str, Union[Trace,CompleteTrace]], Tuple[str, Union[Trace,CompleteTrace], str]] ] = [
    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 5 * M,
         },
        (event('CommitToEquityFinancing', 'Company', 0),
         event('DeliverTransactionDocsWithPRA', 'Company', 0),
         event('IssueSAFEPreferredStock', 'Company', 0,
               {'company_capitalization': 11 * M, 'premoney_valuation': 10 * M}),
         event('DoEquityFinancing', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {"investor_SAFE_Preferred_Stocks": 220000})  # primer says 220,022 from rounding price
        , "Example 1 in SAFE_Primer.rtf"
    ),

    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 4 * M,
         },
        (event('CommitToEquityFinancing', 'Company', 0),
         event('DeliverTransactionDocsWithPRA', 'Company', 0),
         event('IssueSAFEPreferredStock', 'Company', 0,
               {'company_capitalization': 12.5 * M, 'premoney_valuation': 3 * M}),
         event('DoEquityFinancing', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {"investor_SAFE_Preferred_Stocks": 416667}),
        "Example 2 in SAFE_Primer.rtf"
    ),

    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 8 * M,
         },
        (event('CommitToEquityFinancing', 'Company', 0),
         event('DeliverTransactionDocsWithPRA', 'Company', 0),
         event('IssueSAFEPreferredStock', 'Company', 0,
               {'company_capitalization': 11.5 * M, 'premoney_valuation': 8 * M}),
         event('DoEquityFinancing', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {"investor_SAFE_Preferred_Stocks": 143750})  # primer says 143,760 from rounding price
        ,"Example 3 in SAFE_Primer.rtf"
    ),

    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 10 * M,
         },
        (event('CommitToIPO', 'Company', 0, {
            'company_cash_at_liquidity_event': 50 * M,
            'company_capitalization': 11.5 * M,
            'company_valuation': 11 * M
        }),

         event('ChooseCashPayment', 'Investor', 0),
         event('TransferCash_L', 'Company', 0, {'total_investments_of_cashout_investors': 11 * M}),
         # event('TransferCommonStock', 'Company', 0),
         event('DoLiquidityEvent', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {
         "investor_Common_Stocks": 0,
         "investor_SAFE_Preferred_Stocks": 0,
         "investor_cash": 100000}),
        "Example 4 in SAFE_Primer.rtf, if dumb and choose cash payment"
    ),

    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 10 * M,
         },
        (event('CommitToIPO', 'Company', 0, {
            'company_cash_at_liquidity_event': 9 * M,
            'company_capitalization': 11.5 * M,
            'company_valuation': 11 * M, # note greater than CAP
        }),

         event('ChooseCashPayment', 'Investor', 0),
         event('TransferCash_L', 'Company', 0, {'total_investments_of_cashout_investors': 11 * M}),
         event('TransferCommonStock', 'Company', 0),
         event('DoLiquidityEvent', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {
         "investor_Common_Stocks": 20909,
         "investor_SAFE_Preferred_Stocks": 0,
         "liq_cashout": 81818.18181818181,
         "investor_cash": 81818.18181818181}),
         "Example where company can't fully pay out the investors"
    ),

    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 10 * M,
         },
        (event('CommitToIPO', 'Company', 0, {
            'company_cash_at_liquidity_event': 9 * M,
            'company_capitalization': 11.5 * M,
            'company_valuation': 11 * M,
        }),

         event('ChooseCashPayment', 'Investor', 0),
         event('TransferCash_L', 'Company', 0, {'total_investments_of_cashout_investors': 10 * M}),
         event('TransferCommonStock', 'Company', 0),
         event('DoLiquidityEvent', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {
         "investor_Common_Stocks": 11500,
         "investor_SAFE_Preferred_Stocks": 0,
         "investor_cash": 90000}),
        "Example where company can't fully pay out the investors, AND not all investments are from SAFE investors"
    ),

    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 10 * M,
         },
        (event('CommitToIPO', 'Company', 0, {
            'company_cash_at_liquidity_event': 50 * M,
            'company_capitalization': 11.5 * M,
            "company_valuation": 11 * M,

        }),

         event('ChooseStockPayment', 'Investor', 0),
         event('TransferCommonStock', 'Company', 0),
         event('DoLiquidityEvent', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL, {
            "investor_Common_Stocks": 115000
        }), "Example 4 in SAFE_Primer.rtf"
    ),

    ('serious/SAFE_discount.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 20 * K,
         # "VALUATION_CAP": inf,
         "DISCOUNT_RATE": .8
         },
        (event('CommitToEquityFinancing', 'Company', 0),
         event('DeliverTransactionDocsWithPRA', 'Company', 0),
         event('IssueSAFEPreferredStock', 'Company', 0,
               {'company_capitalization': 10.5 * M, 'premoney_valuation': 2 * M}),
         event('DoEquityFinancing', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {"undiscounted_price_per_share_standard_preferred_stock": 2 / 10.5,
         "conversion_price": 0.8 * (2 / 10.5),
         "investor_SAFE_Preferred_Stocks": 131250})  # primer says 131,578 from rounding price
     , "Example 8 in SAFE_Primer.rtf"
     ),

]

# this is used in runme_before_commit.py b/c I often comment out some of the entries of EXAMPLES_TO_RUN
EXAMPLES_FULL_SIZE = sum((len({x[0] for x in col}) for col in [traces_toy_and_teaching, traces_from_academic_lit, traces_serious]))
traces = chain(traces_toy_and_teaching, traces_from_academic_lit, traces_serious)

EXAMPLES_TO_RUN = [
        'from_academic_lit/hvitved_master_sales_agreement_full_without_future_obligations.l4',

        'from_academic_lit/prisacariu_schneider_abdelsadiq_Internet_provision_with_renew.l4',
        'from_academic_lit/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.l4',
        'from_academic_lit/hvitved_lease.l4',

        'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',

        'toy_and_teaching/test_local_vars.l4',
        'toy_and_teaching/minimal_future-actions.l4',
        'toy_and_teaching/minimal_future-actions2.l4',
        'toy_and_teaching/collatz.l4',
        'toy_and_teaching/collatz2.l4',
        'toy_and_teaching/monster_burger_program_only.l4',

        # 'serious/SAFE.l4',
        'serious/SAFE_cap.l4',
        'serious/SAFE_discount.l4',
        # 'serious/SAFE_cap_discount.l4',
        # 'serious/SAFE_mfn.l4',
        # 'serious/SAFE_2_liq_eventtypes.l4'
    ]

# so can run it as a library too, which respects exceptions
def main(examples:Dict[str,L4Contract], verbose=True):
    for trace in traces:
        examplekey = trace[0]
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