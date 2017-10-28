; -*- lisp -*-

; qualified equity financing conversion scenario
; mengwong@legalese.com 20171027

(declare-const initialStock Real)                                              ; pre-conversion capital stock = ordinary + preferred shares
(declare-const equityFinancing_premoney Real)                                  ; the next-round financing's pre-money agreed valuation
(declare-const equityFinancing_fdc      Real)                                  ; the next-round financing's pre-money fully diluted capitalization
(declare-const equityFinancing_pps      Real)                                  ; the next-round financing's price per share for next round investors
(declare-const equityFinancing_value    Real)                                  ; the next-round financing's total amount invested by Series A investors
(declare-const equityFinancing_num_shares_subtotal Real)                       ; the next-round financing's number of shares assigned to incoming Series A investors
(declare-const equityFinancing_num_shares_total Real)                          ; the next-round financing's total number of shares including conversion shares
(declare-const equityFinancing_dilutePre Bool)                                 ; default: false; the SAFE/KISS instruments surprise the Series A investors by diluting them immediately

; the company has issued two types of instruments:
;            a kiss(equity) and                     a safe(cap)
(declare-const kiss_has_cap Bool)      (declare-const safe_has_cap Bool)       ; does the instrument have a cap at all?
(declare-const kiss_cap Real)          (declare-const safe_cap Real)           ; cap
(declare-const kiss_discount Real)     (declare-const safe_discount Real)      ; discount, given in 0--100
(declare-const kiss_pps_fromcap Real)  (declare-const safe_pps_fromcap Real)   ; price per share computed according to the cap
(declare-const kiss_pps_fromdis Real)  (declare-const safe_pps_fromdis Real)   ; price per share computed according to the discount
(declare-const kiss_pps Real)          (declare-const safe_pps Real)           ; effective price per share is the lower of the two
(declare-const kiss_num_shares Real)   (declare-const safe_num_shares Real)    ; number of shares post conversion
(declare-const non_kiss_shares Real)   (declare-const non_safe_shares Real)    ; number of other shares that result from conversion, but are not due to the self-sibling security
(declare-const kiss_value Real)        (declare-const safe_value Real)         ; amount of money invested
(declare-const kiss_post Real)         (declare-const safe_post Real)          ; amount of money the post-conversion shares are newly worth
(declare-const kiss_repayable Bool)    (declare-const kiss_force_include_safe Bool) ; if (not kiss_repayable) then safe includes the kiss in company capitalization.
                                                                               ; kisses always exclude safe by default, unless we force inclusion.

(assert (= safe_has_cap (> safe_cap 0))) (assert (= kiss_has_cap (> kiss_cap 0))) ; syntactic sugar -- set cap to 0 and we'll know what you mean
(assert (<= 0 safe_num_shares))          (assert (<= 0 kiss_num_shares))          ; post conversion, the numbers of safe and kiss shares are nonnegative

;
; business logic
; 

; the security converts to the investment value amount divided by the conversion price per share
(assert (= safe_num_shares (/ safe_value safe_pps)))
(assert (= kiss_num_shares (/ kiss_value kiss_pps)))

; the conversion price per share is the lower of the discounted price per share, and the valuation cap price per share
; of course if the instrument doesn't even have a cap then we just take the discounted price per share
(assert (= safe_pps (ite (and safe_has_cap (< safe_pps_fromcap safe_pps_fromdis)) safe_pps_fromcap safe_pps_fromdis)))
(assert (= kiss_pps (ite (and kiss_has_cap (< kiss_pps_fromcap kiss_pps_fromdis)) kiss_pps_fromcap kiss_pps_fromdis)))

; if the discount is 0 then it is simply the equity financing's price per share paid by the Series A investors
(assert (= safe_pps_fromdis (* (- 1 (/ safe_discount 100)) equityFinancing_pps)))
(assert (= kiss_pps_fromdis (* (- 1 (/ kiss_discount 100)) equityFinancing_pps)))



; the SAFE/KISS price per share may be computed as the valuation cap divided by the fully diluted capitalization (from the point of view of the SAFE/KISS)
; this is the essential set of simultaneous equations: two unknowns, two equations.
(assert (= kiss_pps_fromcap (/ kiss_cap (+ initialStock non_kiss_shares))))
(assert (= safe_pps_fromcap (/ safe_cap (+ initialStock non_safe_shares))))

; after the conversion, the shares should be worth a lot more
(assert (= kiss_post (* kiss_num_shares equityFinancing_pps)))
(assert (= safe_post (* safe_num_shares equityFinancing_pps)))

; the Series A price per share is the premoney valuation divided by the premoney's fully diluted cap
(assert (= equityFinancing_pps (/ equityFinancing_premoney equityFinancing_fdc)))

; what is the Series A premoney's fully diluted cap? well, it depends on whether the Series A investor knows about the SAFE/KISS investors,
; and whether the SAFE/KISS conversions are treated as part of the negotiated pre-money valuation, or not;
; if they're not (dilutePre=false) then the Series A investor immediately gets "diluted" by the conversion
; if they are,   (dilutePre=true)  then the SAFE/KISS investors are diluted by the conversion, and should have pro-rata rights in the triggering round.
(assert (= equityFinancing_fdc (ite equityFinancing_dilutePre
                                    (+ initialStock kiss_num_shares safe_num_shares) ; dilutePre:  more favorable to Series A investors; their pre-money includes the SAFE/KISS investors
                                    initialStock)))                                  ; dilutePost: more favorable to SAFE/KISS investors; the Series A pre-money excludes the SAFE/KISS

; a KISS's fully diluted capitalization excludes Safes
(assert (= non_kiss_shares (ite kiss_force_include_safe safe_num_shares 0)))

; a SAFE's company capitalization includes KISS(Equity) but excludes KISS(Debt)
(assert (= non_safe_shares (ite kiss_repayable 0 kiss_num_shares)))

; incoming series A investors get this many shares
(assert (= equityFinancing_num_shares_subtotal (/ equityFinancing_value equityFinancing_pps)))

; this many Series A shares are issued in total
(assert (= equityFinancing_num_shares_total (+ equityFinancing_num_shares_subtotal safe_num_shares kiss_num_shares)))

;
; now we model the liquidity scenarios as opposed to the conversion scenarios
;

;; Safe cap discount
;; 	(b)	Liquidity Event.  If there is a Liquidity Event before the expiration or termination of this instrument, the Investor will, at its option, either (i) receive a cash payment equal to the Purchase Amount (subject to the following paragraph) or (ii) automatically receive from the Company a number of shares of Common Stock equal to the Purchase Amount divided by the Liquidity Price, if the Investor fails to select the cash option. 

;;	“Liquidity Price” means the price per share equal to the Valuation Cap divided by the Liquidity Capitalization.	

;;	“Liquidity Capitalization” means the number, as of immediately prior to the Liquidity Event, of shares of Capital Stock (on an as-converted basis) outstanding, assuming exercise or conversion of all outstanding vested and unvested options, warrants and other convertible securities,
;; but excluding:
;;     (i) shares of Common Stock reserved and available for future grant under any equity incentive or similar plan;
;;     (ii) this instrument;
;;     (iii) other Safes; and
;;     (iv) convertible promissory notes.

;; KISS
;;  Corporate Transaction.  In the event of a Corporate Transaction prior to the conversion of this KISS pursuant to Section 2.1 or 2.3, at Investor’s election, (i) this KISS shall be converted into that number of Conversion Shares equal to the quotient obtained by dividing the Purchase Price by the Conversion Price; or (ii) the Investor shall be paid the Corporate Transaction Payment, prior and in preference to any distribution of any of the cash or other assets of the Company to holders of the Company’s capital stock by reason of their ownership of such stock.  At least ten (10) days prior to the closing of the Corporate Transaction, the Company shall notify the Investor in writing of the terms of the Corporate Transaction.




; in future, for extra credit, extend this to any number of warrants after the style of KISS and SAFE, so we use a list of convertible security types rather than a single type
