; -*- lisp -*-

(push 1)

(assert (= initialStock 10795000))

(assert (= safe_value     100000))
(assert (= safe_cap      7000000))
(assert (= safe_discount       0))

(assert (= equityFinancing_premoney 200000))
(assert (= equityFinancing_value    200000))

; there is no KISS
(assert (= kiss_value          0))
(assert (= kiss_cap         2000))
(assert (= kiss_discount      25))

(assert (= kiss_repayable false))
(assert (= kiss_force_include_safe false))

(assert (= equityFinancing_dilutePre false))


;; TODO: we should model a maturity conversion for the KISS.

;; •	Investor has purchased a safe for $100,000.  The Valuation Cap is $7,000,000.
;; •	The company grows, generates revenue and becomes cash flow positive, and therefore does not need to raise outside capital.  The company has no acquisition offers and no plans to go public.
;; The safe will remain outstanding until the company has a liquidity event, even if that liquidity event doesn’t happen for years after the original date of purchase. 
