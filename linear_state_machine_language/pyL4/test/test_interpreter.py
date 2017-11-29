from itertools import chain
from typing import Sequence, Tuple, Union, Optional

from src.model.EventsAndTraces import CompleteTrace, Trace, Event, breachSectionId
from src.model.constants_and_defined_types import *
from math import inf

from src.model.util import castid

timestamp = cast(TimeStamp,0)
# reveal_type(timestamp)
def nextTSEvent(action_id:str, role_id:str, params:Optional[Dict[str, ActionParamValue]] = None) -> Event:
    global timestamp
    timestamp = cast(TimeStamp,1)
    return Event(action_id=castid(ActionId,action_id), role_id=castid(RoleId,role_id), timestamp=timestamp,
                 params= cast(ActionParamSubst, params) if params else None)

def sameTSEvent(action_id:str, role_id:str, params:Optional[Dict[str, ActionParamValue]] = None) -> Event:
    params = params or dict()
    return Event(action_id=castid(ActionId,action_id), role_id=castid(RoleId,role_id), timestamp=timestamp,
                 params=cast(ActionParamSubst, params) if params else None)

def event(action_id:str, role_id:str = ENV_ROLE, timestamp:int = 0, params:Optional[Dict[str, ActionParamValue]] = None) -> Event:
    params = params or dict()
    return Event(action_id=castid(ActionId, action_id), role_id=castid(RoleId, role_id), timestamp=cast(TimeStamp,timestamp),
                 params=cast(ActionParamSubst, params) if params else None)

K = 1000
M = 1000000



traces_degenerate : Sequence[ Tuple[str, Union[Trace,CompleteTrace]] ] = (
    ('degenerate/minimal_future-actions.l4', CompleteTrace(
        {},
        (event('Throw', 'I'),
         event('Throw', 'I'),
         event('Throw', 'I'),
         event('Catch', 'I', 0, {'m': 1}),
         event('Catch', 'I', 0, {'m': 3}),
         event('Catch', 'I', 0, {'m': 2}),
         event('EnterFulfilled', 'Env', 0),
         ), breachSectionId())
     ),

    ('degenerate/minimal_future-actions.l4', CompleteTrace(
        {},
        (event('Throw', 'I'),  # n = 1 after
         event('Throw', 'I'),  # n = 2 after
         event('Stand', 'I'),  # n = 3 after
         event('Throw', 'I'),  # n = 4 after
         event('Catch', 'I', 0, {'m': 1}),
         event('Catch', 'I', 0, {'m': 4}),

         event('Catch', 'I', 0, {'m': 2}),
         event('Stand', 'I'),  # n = 5 after


         event('EnterFulfilled', 'Env', 0),
         ), 'Fulfilled')
     ),

    # ('degenerate/minimal_future-actions2.l4', CompleteTrace(
    #     {}, (
    #     event('Throw', 'I', 0, {'n':4}),
    #     event('Throw', 'I', 0, {'n':3}),
    #     event('Throw', 'I', 0, {'n':2}),
    #     event('Throw', 'I', 0, {'n':4}),
    #     event('Catch', 'I', 0, {'m': 3}),
    #     event('Catch', 'I', 0, {'m': 2}),
    #     event('Catch', 'I', 0, {'m': 4}),
    #     event('Catch', 'I', 0, {'m': 4}),
    #     event('EnterFulfilled', 'Env', 0),
    #      ), 'Fulfilled')
    #  ),

    # ('degenerate/minimal_future-actions.l4', CompleteTrace(
    #     {},
    #     (   event('Throw','I'),
    #         event('Throw','I'),
    #         event('Stand','I'),
    #         event('Throw','I'),
    #         event('Catch', 'I', 0, {'m': 1}),
    #         event('Catch', 'I', 0, {'m': 4}),
    #         event('Catch', 'I', 0, {'m': 3}),
    #         event('EnterFulfilled', 'Env', 0),
    #     ), breachSectionId())
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