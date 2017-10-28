; -*- lisp -*-

(push 1)

(assert (= initialStock 11500000))

(assert (= safe_value     100000))
(assert (= safe_cap     10000000))
(assert (= safe_discount       0))

(assert (= equityFinancing_premoney 50000000))
(assert (= equityFinancing_value    2000000))

; there is no KISS
(assert (= kiss_value          0))
(assert (= kiss_cap         2000))
(assert (= kiss_discount      25))

(assert (= kiss_repayable false))
(assert (= kiss_force_include_safe false))

(assert (= equityFinancing_dilutePre false))


;; •	Investor has purchased a safe for $100,000.  The Valuation Cap is $10,000,000.
;; •	Another entity proposes to acquire the company for cash consideration of $50,000,000. The company’s fully-diluted outstanding capital stock immediately prior to the acquisition, including 1,500,000 outstanding options but excluding any unallocated shares in the option pool, is 11,500,000 shares.
;; The investor can choose to have the safe purchase amount returned, or convert the safe into shares of common stock and receive the cash consideration with the other common stockholders.   The safe would convert into 115,008 shares of common stock, based on the “Liquidity Price” of $0.8695 per share (the Liquidity Price is calculated by dividing 10,000,000 by 11,500,000).  When the $50,000,000 deal consideration is allocated pro rata among all of the common stockholders, including the investor (and assuming the outstanding options are all exercised), the investor would receive approximately $495,074.  Since this amount is considerably more than the $100,000 purchase amount, the investor would elect to convert the safe.
