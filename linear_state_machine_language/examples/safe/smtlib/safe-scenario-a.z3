; -*- lisp -*-

(push 1)

(assert (= initialStock 100))

(assert (= equityFinancing_premoney 10000))
(assert (= equityFinancing_value     6000))

(assert (= safe_value     500))
(assert (= safe_cap      3000))
(assert (= safe_discount    0))

; safes are pretty much always nonrepayable, so always a warrant
(assert (= safe_repayable false))

(assert (= kiss_value        200))
(assert (= kiss_cap         2000))
(assert (= kiss_discount       0))

(assert (= kiss_repayable false))


; we follow YC SAFE's example and set a DilutePost style which appears to be more favorable to SAFE/KISS investors
(assert (= equityFinancing_dilutePre false))
