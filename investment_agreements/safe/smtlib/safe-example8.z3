; -*- lisp -*-

(push 1)

(assert (= safe_value      20000))
(assert (= safe_cap            0))
(assert (= safe_discount      20))

(assert (= equityFinancing_value     400000))
(assert (= equityFinancing_premoney 2000000))

(assert (= initialStock 10500000))

; there is no KISS
(assert (= kiss_value          0))
(assert (= kiss_cap         2000))
(assert (= kiss_discount      25))

(assert (= kiss_repayable false))
(assert (= kiss_force_include_safe false))
(assert (= equityFinancing_dilutePre false))

;; •	Investor has purchased a safe for $20,000.  The Discount Rate is 80%.
;; •	The company has negotiated with investors to sell $400,000 worth of Series AA Preferred Stock at a $2,000,000 pre-money valuation.  The company’s fully-diluted outstanding capital stock immediately prior to the financing is 10,500,000 shares.
;; The company will issue and sell 2,105,263 shares of Series AA Preferred at $0.19 per share to the new investors. The 20% discount applied to the per share price of the Series AA Preferred is $0.152. Accordingly, the company will issue 131,578 shares of Series AA-1 Preferred to the safe holder, at $0.152 per share. 
