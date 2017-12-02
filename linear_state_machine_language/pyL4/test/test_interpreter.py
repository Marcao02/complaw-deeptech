from itertools import chain
from typing import Sequence, Tuple, Union, Optional
from math import inf

from interpreter import evalTrace
from src.model.L4Contract import L4Contract
from src.parse_sexpr import prettySExprStr, parse_file
from src.sexpr_to_L4Contract import L4ContractConstructor
from src.model.EventsAndTraces import CompleteTrace, Trace, Event, breachSectionId, EventType
from src.model.constants_and_defined_types import *

from src.model.util import castid

def event(action_id:str, role_id:str = ENV_ROLE,
          timestamp:int = 0, params:Optional[Dict[str, Data]] = None,
          eventType: Optional[EventType] = None ) -> Event:
    if eventType is None:
        eventType = EventType.env_next if role_id == ENV_ROLE else EventType.party_next
    params = params or dict()
    return Event(action_id=castid(ActionId, action_id), role_id=castid(RoleId, role_id), timestamp=timestamp,
                 params_by_abap_name=cast(ABAPNamedSubst, params) if params else None,
                 params=list(params.values()) if params else None,
                 type=eventType)

def foevent(action_id:str, role_id:str = ENV_ROLE,
          timestamp:int = 0, params:Optional[Dict[str, Data]] = None) -> Event:
    return event(action_id,role_id,timestamp,params,EventType.fulfill_floating_obligation)

def fpevent(action_id:str, role_id:str = ENV_ROLE,
          timestamp:int = 0, params:Optional[Dict[str, Data]] = None) -> Event:
    return event(action_id,role_id,timestamp,params,EventType.use_floating_permission)

inc_timestamp = 0

def nextTSEvent(action_id:str, role_id:str, params:Optional[Dict[str, Data]] = None,
                eventType: Optional[EventType] = None) -> Event:
    global inc_timestamp
    inc_timestamp = inc_timestamp + 1
    return event(action_id, role_id, inc_timestamp, params, eventType)

def sameTSEvent(action_id:str, role_id:str,
                params:Optional[Dict[str, Data]] = None,
                eventType: Optional[EventType] = None) -> Event:
    global inc_timestamp
    return event(action_id, role_id, inc_timestamp, params, eventType)



K = 1000
M = 1000000



traces_degenerate : Sequence[ Tuple[str, Union[Trace,CompleteTrace]] ] = (
    ('degenerate/minimal_future-actions.l4', CompleteTrace(
        {},
        (event('Throw', 'I'),
         event('Throw', 'I'),
         event('Throw', 'I'),
         foevent('Catch', 'I', 0, {'m': 1}),
         foevent('Catch', 'I', 0, {'m': 3}),
         foevent('Catch', 'I', 0, {'m': 2}),
         event('EnterFulfilled', 'Env', 0),
         ), breachSectionId('Env'))
     ),

    ('degenerate/minimal_future-actions.l4', CompleteTrace(
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
         ), 'Fulfilled')
     ),

    ('degenerate/minimal_future-actions2.l4', CompleteTrace(
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
         ), 'Fulfilled')
     ),

    # ('degenerate/minimal_future-actions.l4', CompleteTrace(
    #     {},
    #     (   event('Throw','I'),
    #         event('Throw','I'),
    #         event('Stand','I'),
    #         event('Throw','I'),
    #         foevent('Catch', 'I', 0, {'m': 1}),
    #         foevent('Catch', 'I', 0, {'m': 4}),
    #         foevent('Catch', 'I', 0, {'m': 3}),
    #         # event('EnterFulfilled', 'Env', 0),
    #     ), breachSectionId('I'))
    # ),


    ('degenerate/collatz.l4', CompleteTrace(
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
         ), FULFILLED_SECTION_LABEL)
     ),

)

traces_toy_and_teaching : Sequence[ Tuple[str, Union[Trace,CompleteTrace]] ] = (

    ('toy_and_teaching/monster_burger_program_only.l4', CompleteTrace({},(
        # start section implicit
        nextTSEvent('RequestCookMB', 'Challenger'),
        nextTSEvent('ServeMB', 'Restaurant'),
        nextTSEvent('AnnounceMBFinished', 'Challenger'),
        nextTSEvent('CheckCompletionClaim', 'Restaurant'),
        sameTSEvent('RejectCompletionClaim', 'Restaurant'),
        sameTSEvent('EnterEatingMB', 'Env'),
        nextTSEvent('AnnounceMBFinished','Challenger'),
        nextTSEvent('CheckCompletionClaim', 'Restaurant'),
        sameTSEvent('VerifyCompletionClaim', 'Restaurant')
        ), FULFILLED_SECTION_LABEL)
     ),
)
traces_from_academic_lit: Sequence[Tuple[str, Union[Trace, CompleteTrace]]] = (


    ('from_academic_lit/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.l4', CompleteTrace(
        {
            'MAX_UNITS' : 1000,
            'CONTRACT_LIFE' : 365,
            'PRICE_PER_UNIT' : 100
        },(
        # start section implicit
        event('SubmitNewOrder', 'Customer', 5, {'quantity':300}), # orderid 0
        event('SubmitNewOrder', 'Customer', 6, {'quantity': 200}),  # orderid 1
        foevent('Deliver', 'Vendor', 10, {'quantity':200,'orderid':1}),
        foevent('Deliver', 'Vendor', 19, {'quantity':300,'orderid':0}),
        fpevent('EmailInvoice', 'Vendor', 40, {'quantity':200,'orderid':1}),
        event('SubmitNewOrder', 'Customer', 41, {'quantity': 500}),  # orderid 2
        foevent('Deliver', 'Vendor', 42, {'quantity':500,'orderid':2}),
        # event('EnterFulfilled', 'Env', 50)
        ), breachSectionId('Customer'))
        # It should end in a breach by Customer due to the unpaid invoice.
    ),

    ('from_academic_lit/hvitved_instalment_sale--simplified_time.l4', CompleteTrace({},(
        # start section implicit
        event('PayInstallment', 'Buyer', 30, {'amount':500}),
        event('PayInstallment', 'Buyer', 60, {'amount':500}),
        event('PayInstallment', 'Buyer', 90, {'amount':8000}),
        event('PayLastInstallment', 'Buyer', 120, {'amount':1000}),
        ), FULFILLED_SECTION_LABEL)
    ),

    ('from_academic_lit/hvitved_instalment_sale--simplified_time.l4', CompleteTrace({},(
        event('PayInstallment', 'Buyer', 30, {'amount':499}),
        ), breachSectionId('Buyer'))
    ),

    ('from_academic_lit/hvitved_instalment_sale--simplified_time.l4', CompleteTrace({},(
        event('PayInstallment', 'Buyer', 30, {'amount':500}),
        event('PayInstallment', 'Buyer', 60, {'amount':500}),
        event('PayInstallment', 'Buyer', 90, {'amount':7999}),
        event('PayLastInstallment', 'Buyer', 120, {'amount':1000}),
        ), breachSectionId('Buyer'))
    ),

    ('from_academic_lit/hvitved_instalment_sale--simplified_time.l4', CompleteTrace({},(
        event('PayInstallment', 'Buyer', 30, {'amount':500}),
        event('PayInstallment', 'Buyer', 60, {'amount':500}),
        event('PayInstallment', 'Buyer', 90, {'amount':8500}),
        event('PayLastInstallment', 'Buyer', 120, {'amount':500}),
        ), FULFILLED_SECTION_LABEL)
    ),

)


traces_serious: Sequence[ Tuple[str, Union[Trace,CompleteTrace]] ] = (
    ('serious/SAFE.l4', CompleteTrace(
        # Example 1 in SAFE_Primer.rtf
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 5 * M,
         "DISCOUNT_RATE": 1
         },
        (event('CommitToEquityFinancing', 'Company', 0),
         event('DeliverDocsWithPRA', 'Company', 0),
         event('IssueSAFEPreferredStock', 'Company', 0,
               {'company_capitalization': 11 * M, 'premoney_valuation': 10 * M}),
         event('DoEquityFinancing', 'Company', 0)
         ),
        FULFILLED_SECTION_LABEL,
        {"investor_SAFE_Preferred_Stocks": 220000})  # primer says 220,022 from rounding price
     ),

    ('serious/SAFE.l4', CompleteTrace(
        # Example 2 in SAFE_Primer.rtf
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 4 * M,
         "DISCOUNT_RATE": 1
         },
        (event('CommitToEquityFinancing', 'Company', 0),
         event('DeliverDocsWithPRA', 'Company', 0),
         event('IssueSAFEPreferredStock', 'Company', 0,
               {'company_capitalization': 12.5 * M, 'premoney_valuation': 3 * M}),
         event('DoEquityFinancing', 'Company', 0)
         ),
        FULFILLED_SECTION_LABEL,
        {"investor_SAFE_Preferred_Stocks": 416666})
     ),

    ('serious/SAFE.l4', CompleteTrace(
        # Example 3 in SAFE_Primer.rtf
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 8 * M,
         "DISCOUNT_RATE": 1
         },
        (event('CommitToEquityFinancing', 'Company', 0),
         event('DeliverDocsWithPRA', 'Company', 0),
         event('IssueSAFEPreferredStock', 'Company', 0,
               {'company_capitalization': 11.5 * M, 'premoney_valuation': 8 * M}),
         event('DoEquityFinancing', 'Company', 0)
         ),
        FULFILLED_SECTION_LABEL,
        {"investor_SAFE_Preferred_Stocks": 143750})  # primer says 143,760 from rounding price
     ),

    ('serious/SAFE.l4', CompleteTrace(
        # Example 4 in SAFE_Primer.rtf
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 10 * M,
         "DISCOUNT_RATE": 1
         },
        (event('CommitToLiquidityEvent', 'Company', 0, {
            'change_of_control': False,
            'company_cash_at_liquidity_event': 50 * M,
            'liquidity_capitalization': 11.5 * M,
            'reduction_needed_to_qualify_as_usa_tax_free_reorg': 0
        }),

         event('ChooseStockPayment', 'Investor', 0),
         event('TransferCommonStock', 'Company', 0),
         event('DoLiquidityEvent', 'Company', 0)
         ),
        FULFILLED_SECTION_LABEL, {
            "investor_Common_Stocks": 115000
        })
     ),

    ('serious/SAFE.l4', CompleteTrace(
        # Example 4 in SAFE_Primer.rtf, if dumb and choose cash payment
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 10 * M,
         "DISCOUNT_RATE": 1
         },
        (event('CommitToLiquidityEvent', 'Company', 0, {
            'change_of_control': False,
            'company_cash_at_liquidity_event': 50 * M,
            'liquidity_capitalization': 11.5 * M,
            'reduction_needed_to_qualify_as_usa_tax_free_reorg': 0
        }),

         event('ChooseCashPayment', 'Investor', 0),
         event('TransferCash_L', 'Company', 0),
         # event('TransferCommonStock', 'Company', 0),
         event('DoLiquidityEvent', 'Company', 0)
         ),
        FULFILLED_SECTION_LABEL,
        {"investor_liq_hypothetical_shares": 115000,
         "investor_Common_Stocks": 0,
         "investor_SAFE_Preferred_Stocks": 0,
         "investor_cash": 100000})
     ),

    ('serious/SAFE.l4', CompleteTrace(
        # Example 8 in SAFE_Primer.rtf
        {"PURCHASE_AMOUNT": 20 * K,
         "VALUATION_CAP": inf,
         "DISCOUNT_RATE": .8
         },
        (event('CommitToEquityFinancing', 'Company', 0),
         event('DeliverDocsWithPRA', 'Company', 0),
         event('IssueSAFEPreferredStock', 'Company', 0,
               {'company_capitalization': 10.5 * M, 'premoney_valuation': 2 * M}),
         event('DoEquityFinancing', 'Company', 0)
         ),
        FULFILLED_SECTION_LABEL,
        {"initial_price_per_share_standard_preferred_stock": 2 / 10.5,
         "discount_price": 0.8 * (2 / 10.5),
         "investor_SAFE_Preferred_Stocks": 131250})  # primer says 131,578 from rounding price
     )

)

traces = chain(traces_degenerate, traces_from_academic_lit, traces_serious, traces_toy_and_teaching)

from src.cli import EXAMPLES_SEXPR_ROOT

# so can run it as a library too, which respects exceptions
def main(sys_argv:List[str]):

    EXAMPLES_TO_RUN = [
        # 'from_academic_lit/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.l4',
        'degenerate/minimal_future-actions.l4',
        'degenerate/minimal_future-actions2.l4',
        'toy_and_teaching/monster_burger_program_only.l4',
        'from_academic_lit/hvitved_instalment_sale--simplified_time.l4',
        'degenerate/collatz.l4',
        'serious/SAFE.l4',
    ]
    for trace in traces:
        subpath = trace[0]
        if subpath in EXAMPLES_TO_RUN:
            print("\n" + subpath)
            path = EXAMPLES_SEXPR_ROOT + subpath
            parsed = parse_file(path)
            if 'print' in sys_argv:
                print(prettySExprStr(parsed))

            assembler = L4ContractConstructor(path)
            prog : L4Contract = assembler.mk_l4contract(parsed)

            evalTrace(trace[1], prog)


if __name__ == '__main__':
    import sys
    main(sys.argv)


                # 'examples/hvitved_master_sales_agreement_full_with_ids.LSM': [
    #     nextTSEvent('Start'),
    #     nextTSEvent('ContractLive'),
    #     nextTSEvent('NewOrder',[50]),
    #     nextTSEvent('ContractLive'),
    #     nextTSEvent('NewOrder',[40]),
    # ],
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