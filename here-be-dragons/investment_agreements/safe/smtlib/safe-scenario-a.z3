; -*- lisp -*-

(push 1)

(assert (= initialStock 100))

(assert (= equityFinancing_premoney 10000))
(assert (= equityFinancing_value     6000))

(assert (= safe_value     500))
(assert (= safe_cap      3000))
(assert (= safe_discount    0))

(assert (= kiss_value        200))
(assert (= kiss_cap         2000))
(assert (= kiss_discount       0))

; let's exclude the safe from the kiss's notion of FDC
; and   exclude the kiss from the safe's notion of FDC
(assert (= kiss_repayable false))
(assert (= kiss_force_include_safe false))
; this should cause converting investors to end up with a smaller percentage of the company

; we follow YC SAFE's example and set a DilutePost style which appears to be more favorable to SAFE/KISS investors
(assert (= equityFinancing_dilutePre false))
