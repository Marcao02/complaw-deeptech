; -*- lisp -*-

; qualified equity financing conversion scenario
; mengwong@legalese.com 20171027

; cap only, no discount
; here we examine the cap scenario

(declare-const initialStock Real)                                              ; pre-conversion capital stock = ordinary + preferred shares

; the company has issued two types of instruments:
; a            kiss(equity) and                     a safe(cap)
(declare-const kiss_cap Real)          (declare-const safe_cap Real)           ; cap
(declare-const kiss_discount Real)     (declare-const safe_discount Real)      ; discount
(declare-const safe_pps Real)          (declare-const kiss_pps Real)           ; price per share
(declare-const safe_num_shares Real)   (declare-const kiss_num_shares Real)    ; number of shares post conversion
(declare-const safe_value Real)        (declare-const kiss_value Real)         ; amount of money invested

; post conversion, the numbers of safe and kiss shares are positive
(assert (< 0 safe_num_shares))         (assert (< 0 kiss_num_shares))

; the system of simultaneous equations
(assert (= safe_num_shares (/ safe_value safe_pps)))
(assert (= kiss_num_shares (/ kiss_value kiss_pps)))

(assert (= kiss_pps (/ kiss_cap (+ initialStock safe_num_shares))))
(assert (= safe_pps (/ safe_cap (+ initialStock kiss_num_shares))))

; in future, for extra credit, extend this to any number of warrants after the style of KISS and SAFE, so we use a list of convertible security types rather than a single type

(push 1)
