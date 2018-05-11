# ---------------Examples from SAFE primer---------------
from copy import deepcopy

from test.test_util import *

TOO_LARGE_CAP = 99999999

primer_ex = [
    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 5 * M,
         },
        (event('TransferInvestmentCash', "Investor",0),
         event('CommitToEquityFinancing', 'Company', 0),
         event('DeliverTransactionDocsWithPRA', 'Company', 0),
         event('IssueSAFEPreferredStock', 'Company', 0,
               {'company_capitalization': 11 * M, 'premoney_valuation': 10 * M}),
         event('DoEquityFinancing', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {"investor_SAFE_Stock_delta": 220000})  # primer says 220,022 from rounding price
        , "Example 1 in SAFE_Primer.rtf"
    ),

    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 4 * M,
         },
        (event('TransferInvestmentCash', "Investor",0),
         event('CommitToEquityFinancing', 'Company', 0),
         event('DeliverTransactionDocsWithPRA', 'Company', 0),
         event('IssueSAFEPreferredStock', 'Company', 0,
               {'company_capitalization': 12.5 * M, 'premoney_valuation': 3 * M}),
         event('DoEquityFinancing', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {"investor_SAFE_Stock_delta": 416667}),
        "Example 2 in SAFE_Primer.rtf"
    ),

    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 8 * M,
         },
        (event('TransferInvestmentCash', "Investor",0),
         event('CommitToEquityFinancing', 'Company', 0),
         event('DeliverTransactionDocsWithPRA', 'Company', 0),
         event('IssueSAFEPreferredStock', 'Company', 0,
               {'company_capitalization': 11.5 * M, 'premoney_valuation': 8 * M}),
         event('DoEquityFinancing', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {"investor_SAFE_Stock_delta": 143750})  # primer says 143,760 from rounding price
        ,"Example 3 in SAFE_Primer.rtf"
    ),

    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 10 * M,
         },
        (event('TransferInvestmentCash', "Investor",0),
         event('CommitToIPO', 'Company', 0, {
            'company_capitalization': 11.5 * M,
            'company_valuation': 11 * M
        }),

         event('ChooseCashPayment', 'Investor', 0),
         event('TransferCash_L', 'Company', 0, {
             'company_cash_at_liquidity_event': 50 * M,
             'total_investments_of_cashout_investors': 11 * M}),
         # event('TransferCommonStock', 'Company', 0),
         event('DoLiquidityEvent', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {
         "investor_Common_Stock_delta": 0,
         "investor_SAFE_Stock_delta": 0,
         "investor_cash_delta": 100*K - 100*K}),
        "Example 4 in SAFE_Primer.rtf, if dumb and choose cash payment"
    ),

    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 10 * M,
         },
        (event('TransferInvestmentCash', "Investor",0),
         event('CommitToIPO', 'Company', 0, {
            'company_capitalization': 11.5 * M,
            "company_valuation": 11 * M,
        }),

         event('ChooseStockPayment', 'Investor', 0),
         event('TransferCommonStock', 'Company', 0),
         event('DoLiquidityEvent', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL, {
            "investor_Common_Stock_delta": 115000
        }), "Example 4 in SAFE_Primer.rtf"
    ),

    ('serious/SAFE_discount.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 20 * K,
         # "VALUATION_CAP": inf,
         "DISCOUNT_RATE": .8
         },
        (event('TransferInvestmentCash', "Investor",0),
         event('CommitToEquityFinancing', 'Company', 0),
         event('DeliverTransactionDocsWithPRA', 'Company', 0),
         event('IssueSAFEPreferredStock', 'Company', 0,
               {'company_capitalization': 10.5 * M, 'premoney_valuation': 2 * M}),
         event('DoEquityFinancing', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {#"undiscounted_standard_preferred_stock_price": 2 / 10.5,
         "conversion_price": 0.8 * (2 / 10.5),
         "investor_SAFE_Stock_delta": 131250})  # primer says 131,578 from rounding price
     , "Example 8 in SAFE_Primer.rtf"
     )
]

other_ex = [
    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 10 * M,
         },

        (event('TransferInvestmentCash', "Investor",0),
         event('CommitToIPO', 'Company', 0, {

            'company_capitalization': 11.5 * M,
            'company_valuation': 11 * M, # note greater than CAP
           }),

         event('ChooseCashPayment', 'Investor', 0),
         event('TransferCash_L', 'Company', 0, {
             'company_cash_at_liquidity_event': 9 * M,
             'total_investments_of_cashout_investors': 11 * M}),
         event('TransferCommonStock', 'Company', 0),
         event('DoLiquidityEvent', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {
         "investor_Common_Stock_delta": 20910,
         "investor_SAFE_Stock_delta": 0,
         "liq_cashout": 81818.18181818181,
         "investor_cash_delta": 81818.18181818181 - 100*K}),
         "Example where company can't fully pay out the investors"
    ),

    ('serious/SAFE_cap.l4', CompleteTrace(
        {"PURCHASE_AMOUNT": 100 * K,
         "VALUATION_CAP": 10 * M,
         },
        (event('TransferInvestmentCash', "Investor",0),
         event('CommitToIPO', 'Company', 0, {
            'company_capitalization': 11.5 * M,
            'company_valuation': 11 * M,
          }),
         event('ChooseCashPayment', 'Investor', 0),
         event('TransferCash_L', 'Company', 0, {
             'company_cash_at_liquidity_event': 9 * M,
             'total_investments_of_cashout_investors': 10 * M}),
         event('TransferCommonStock', 'Company', 0),
         event('DoLiquidityEvent', 'Company', 0)
         ),
        FULFILLED_SITUATION_LABEL,
        {
         "investor_Common_Stock_delta": 11500,
         "investor_SAFE_Stock_delta": 0,
         "investor_cash_delta": 90000 - 100*K}),

         "Example where company can't fully pay out the investors, AND not all investments are from SAFE investors"
    )

]


primer_derived = []
for case in primer_ex:
    if len(case) == 3:
        case2 = ("serious/SAFE_cap_discount.l4", deepcopy(case[1]), case[2])
    else:
        case2 = ("serious/SAFE_cap_discount.l4", deepcopy(case[1]), "")
    if "SAFE_discount.l4" in case[0]:
        case2[1].contract_param_subst["VALUATION_CAP"] = TOO_LARGE_CAP  # wanted this to cause a test error
    elif "SAFE_cap.l4" in case[0]:
        case2[1].contract_param_subst["DISCOUNT_RATE"] = 1 # wanted this to cause a test error

    primer_derived.append(case2)

almost_all_safe_tests = primer_ex + other_ex + primer_derived
all_safe_tests = []
# for case in almost_all_safe_tests:
#     if "SAFE_cap_discount.l4" in case[0]:
#         if len(case) == 3:
#             all_safe_tests.append(("serious/wip/SAFE-nlg/SAFE_nlg_compatible_cap_discount.l4", deepcopy(case[1]), case[2]))
#         else:
#             all_safe_tests.append(("serious/wip/SAFE-nlg/SAFE_nlg_compatible_cap_discount.l4", deepcopy(case[1])))
all_safe_tests.extend(almost_all_safe_tests)

