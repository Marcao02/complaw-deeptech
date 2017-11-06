; -*- lisp -*-

(push 1)

; we know that there were 11,000,000 shares to start with
(assert (= initialStock 11000000))

; new investors want to put money in at a $10,000,000 premoney valuation
(assert (= equityFinancing_premoney 10000000))
(assert (= equityFinancing_value     1000000))

; the SAFE raised $100,000 at a $5M cap, no discount
(assert (= safe_value     100000))
(assert (= safe_cap      5000000))
(assert (= safe_discount       0))

; there is no KISS
(assert (= kiss_value          0))
(assert (= kiss_cap         2000))
(assert (= kiss_discount      25))

; kiss(debt)   is repayable and thus a convertible promissory note, and excluded from a safe's notion of company capitalization.
; kiss(equity) is not repayable and thus a warrant, and included in a safe's notion of company capitalization.
(assert (= kiss_repayable false))

; safes are pretty much always nonrepayable, unless we force an override
(assert (= kiss_force_include_safe false))

; we follow YC SAFE's example and set a DilutePost style which appears to be more favorable to SAFE/KISS investors
(assert (= equityFinancing_dilutePre false))

;; Example 1
;; •	Investor has purchased a safe for $100,000.  The Valuation Cap is $5,000,000.
;; •	The company negotiates with investors to sell $1,000,000 worth of Series A Preferred Stock at a $10,000,000 pre-money valuation.  The company’s fully-diluted outstanding capital stock immediately prior to the financing, including a 1,000,000 share option pool to be adopted in connection with the financing, is 11,000,000 shares.
;; The company will issue and sell 1,100,110 shares of Series A Preferred at $0.909 per share to the new investors.  The company will issue and sell 220,022 shares of Series A-1 Preferred to the safe holder, at $0.4545 per share.


