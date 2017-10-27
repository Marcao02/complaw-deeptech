; -*- lisp -*-

(push 1)

; we know that there were 12,500,000 shares to start with
(assert (= initialStock 12500000))

; new investors want to put money in at a $3,000,000 premoney valuation
(assert (= equityFinancing_premoney 3000000))
(assert (= equityFinancing_value     600000))

; the SAFE raised $100,000 at a $4M cap, no discount
(assert (= safe_value     100000))
(assert (= safe_cap      5000000))
(assert (= safe_discount       0))

; safes are pretty much always nonrepayable, so always a warrant
(assert (= safe_repayable false))

; there is no KISS
(assert (= kiss_value          0))
(assert (= kiss_cap         2000))
(assert (= kiss_discount      25))

; kiss(debt)   is repayable and thus a convertible promissory note
; kiss(equity) is not repayable and thus a warrant.
(assert (= kiss_repayable false))


; we follow YC SAFE's example and set a DilutePost style which appears to be more favorable to SAFE/KISS investors
(assert (= equityFinancing_dilutePre false))

;; •	Investor has purchased a safe for $100,000.  The Valuation Cap is $4,000,000.
;; •	The company negotiates with investors to sell $600,000 worth of Series AA Preferred Stock at a $3,000,000 pre-money valuation.  The company’s fully-diluted outstanding capital stock immediately prior to the financing, including a 500,000 share option pool to be adopted in connection with the financing, is 12,500,000 shares.
;; The company will issue and sell 2,500,000 shares of Series AA Preferred at $0.24 per share to the new investors.  The Company will issue and sell an additional 416,666 shares of Series AA Preferred to the safe holder at the same price per share.  There is no difference between the Series AA Preferred issued to the new investors and the Series AA Preferred issued to the safe holder, and the references to “Safe Preferred Stock” in this case are inapplicable.
