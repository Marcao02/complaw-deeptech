; -*- lisp -*-

(push 1)

(assert (= initialStock 11500000))

(assert (= safe_value     100000))
(assert (= safe_cap      8000000))
(assert (= safe_discount       0))
(assert (= safe_repayable false))

(assert (= equityFinancing_premoney 8000000))
(assert (= equityFinancing_value    2000000))

; there is no KISS
(assert (= kiss_value          0))
(assert (= kiss_cap         2000))
(assert (= kiss_discount      25))
(assert (= kiss_repayable false))

(assert (= equityFinancing_dilutePre false))

;; •	Investor has purchased a safe for $100,000.  The Valuation Cap is $8,000,000.
;; •	The company negotiates with investors to sell $2,000,000 worth of Series A Preferred Stock at an $8,000,000 pre-money valuation.  The company’s fully-diluted outstanding capital stock immediately prior to the financing, including a 1,500,000 share option pool to be adopted in connection with the financing, is 11,500,000 shares.
;; The company will issue and sell 2,875,215 shares of Series A Preferred at $0.6956 per share to the new investors.  The company will issue and sell an additional 143,760 shares of Series A Preferred to the safe holder at the same price per share.  As in Example 2, there is no difference between the Series A Preferred issued to the new investors and the Series A Preferred issued to the safe holder.
