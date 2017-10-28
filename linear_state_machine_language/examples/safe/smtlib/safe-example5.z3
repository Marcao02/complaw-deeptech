; -*- lisp -*-

(push 1)

(assert (= initialStock 10795000))

(assert (= safe_value     100000))
(assert (= safe_cap      6000000))
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

;; •	Investor has purchased a safe for $100,000.  The Valuation Cap is $6,000,000.
;; •	Another entity proposes to acquire the company for cash consideration of $200,000. The company’s fully-diluted outstanding capital stock immediately prior to the acquisition, including 795,000 outstanding options but excluding any unallocated shares in the option pool, is 10,795,000 shares.
;; The investor can choose to have the safe purchase amount returned, or convert the safe into shares of common stock and participate pro rata in the cash consideration with the other common stockholders.   The safe would convert into 179,920 shares of common stock, based on the “Liquidity Price” of $0.5558 per share (the Liquidity Price is calculated by dividing 6,000,000 by 10,795,000).  When the $200,000 deal consideration is allocated pro rata among all of the common stockholders, including the investor (and assuming: (1) the outstanding options are all exercised; (2) there is no outstanding debt; and (3) for purposes of this example, there is only the one outstanding safe), the investor would receive approximately $3,274.  This dollar amount is calculated by dividing the $200,000 deal consideration among 10,974,920 shares of outstanding common stock, resulting in $0.0182 per share (179,920 shares multiplied by $0.0182 = $3,274.54).  Since this amount is considerably less than the $100,000 purchase amount, the investor would elect to cash out the safe in connection with the transaction.
