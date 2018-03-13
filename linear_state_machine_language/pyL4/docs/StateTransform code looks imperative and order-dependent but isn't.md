e.g. all of the following lines can be permuted within their blocks without changing the meaning:

```
(company_cash' = company_cash_at_liquidity_event)
(liq_price' = (VALUATION_CAP / liquidity_capitalization))
(investor_liq_hypothetical_shares' = (PURCHASE_AMOUNT round/ liq_price'))
(investor_percent_of_cashout_investor_investments' = (fraction-of-sum investor_liq_hypothetical_shares' liquidity_capitalization))
(if  (change_of_control and (reduction_needed_to_qualify_as_usa_tax_free_reorg > ($ 0))) (
 	(local purchase_amount_reduction : $ = (investor_percent_of_cashout_investor_investments' * reduction_needed_to_qualify_as_usa_tax_free_reorg))
	(if (purchase_amount_reduction ≤ PURCHASE_AMOUNT) (
	 	(investor_Common_Stocks' += (purchase_amount_reduction round/ liq_price'))
		(cash_currently_unconverted' = (check $ (PURCHASE_AMOUNT - purchase_amount_reduction)))
	)
	else (
		(investor_Common_Stocks' += (PURCHASE_AMOUNT round/ liq_price'))
		(cash_currently_unconverted' = 0)
	))
)
else (
	; This doesn't actually need to be a separate case, but I think code is more readable when it is.
	(cash_currently_unconverted' = PURCHASE_AMOUNT)
))
```			