; -*- lisp -*-

; we know that there were 100 shares to start with
(assert (= initialStock 100))

; the SAFE raised $200 at a $2000 cap, 0% discount
(assert (= safe_value 300))
(assert (= safe_cap 2000))
(assert (= safe_discount 0))

; the KISS raised $200 at a $2000 cap, 0% discount
(assert (= kiss_value 200))
(assert (= kiss_cap 2000))
(assert (= kiss_discount 0))
