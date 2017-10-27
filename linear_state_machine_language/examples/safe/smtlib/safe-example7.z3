; -*- lisp -*-

(push 1)

(assert (= safe_value     100000))
(assert (= safe_cap      8000000))
(assert (= safe_discount      15))
(assert (= safe_repayable false))

(assert (= equityFinancing_value     1000000))
(assert (= equityFinancing_premoney 10000000))

(assert (= initialStock 11000000))

; there is no KISS
(assert (= kiss_value          0))
(assert (= kiss_cap         2000))
(assert (= kiss_discount      25))
(assert (= kiss_repayable false))

(assert (= equityFinancing_dilutePre false))

;; Investor has purchased a safe for $100,000.  The Valuation Cap is $8,000,000 and the Discount Rate is 85%.
;; •	The company has negotiated with investors to sell $1,000,000 worth of Series A Preferred Stock at a $10,000,000 pre-money valuation.  The company’s fully-diluted outstanding capital stock immediately prior to the financing, including a 1,000,000 share option pool to be adopted in connection with the financing, is 11,000,000 shares.
;; The company will issue and sell 1,100,110 shares of Series A Preferred at $0.909 per share to the new investors.  The company will issue Series A-1 Preferred to the safe holder, based on the Valuation Cap or the Discount Rate, whichever results in a lower price per share. The 15% discount applied to the per share price of the Series A Preferred is $0.77265. The Valuation Cap results in a price per share of $0.72727.  Accordingly, the company will issue 137,500 shares of Series A-1 Preferred to the safe holder, at $0.72727 per share. The Discount Rate does not apply in this case.
