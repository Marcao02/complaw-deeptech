; -*- lisp -*-

; we follow YC SAFE's example and set a DilutePost style which appears to be more favorable to SAFE/KISS investors
(assert (= equityFinancing_dilutePre false))

; we know that there were 100 shares to start with
(assert (= initialStock 100))

; new investors want to put money in at a $10,000 premoney valuation
(assert (= equityFinancing_premoney 10000))

; the SAFE raised $200 at a $2000 cap, 25% discount
(assert (= safe_value 300))
(assert (= safe_cap 2000))
(assert (= safe_discount 20))

; safes are pretty much always nonrepayable, so always a warrant
(assert (= safe_repayable false))

; the KISS raised $200 at a $2000 cap, 25% discount
(assert (= kiss_value 200))
(assert (= kiss_cap 2000))
(assert (= kiss_discount 25))

; kiss(debt)   is repayable and thus a convertible promissory note
; kiss(equity) is not repayable and thus a warrant.
(assert (= kiss_repayable false))


