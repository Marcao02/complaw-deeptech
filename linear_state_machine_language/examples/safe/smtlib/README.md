# SMTLIB solutions to SAFE and KISS interactions

This Z3 SMTlib2 code works through any scenario in which a startup raises using both SAFE and and KISS instruments, and subsequently raises equity and needs to do the conversion.

## Usage

Copy the safe-example1.z3 to your particular situation.

## Example

```
20171027-23:44:37 mengwong@venice2:~/non-db-src/l/compiler/linear_state_machine_language/examples/safe/smtlib% cat safe-pre.z3 safe-example1.z3 safe-post.z3 | z3 -in
sat
(model
  (define-fun kiss_repayable             () Bool  false)
  (define-fun kiss_discount              () Real  25.0)
  (define-fun kiss_cap                   () Real  2000.0)
  (define-fun kiss_value                 () Real  200.0)
  (define-fun safe_repayable             () Bool  false)
  (define-fun safe_discount              () Real  20.0)
  (define-fun safe_cap                   () Real  2000.0)
  (define-fun safe_value                 () Real  300.0)
  (define-fun equityFinancing_premoney   () Real  10000.0)
  (define-fun initialStock               () Real  100.0)
  (define-fun equityFinancing_dilutePre  () Bool  false)
  (define-fun non_safe_shares            () Real  (/ 2300.0 197.0))
  (define-fun non_kiss_shares            () Real  (/ 3300.0 197.0))
  (define-fun equityFinancing_fdc        () Real  100.0)
  (define-fun equityFinancing_pps        () Real  100.0)
  (define-fun kiss_pps_fromdis           () Real  75.0)
  (define-fun safe_pps_fromdis           () Real  80.0)
  (define-fun kiss_pps                   () Real  (/ 394.0 23.0))
  (define-fun kiss_pps_fromcap           () Real  (/ 394.0 23.0))
  (define-fun safe_pps                   () Real  (/ 197.0 11.0))
  (define-fun safe_pps_fromcap           () Real  (/ 197.0 11.0))
  (define-fun kiss_num_shares            () Real  (/ 2300.0 197.0))
  (define-fun safe_num_shares            () Real  (/ 3300.0 197.0))
  (define-fun kiss_has_cap               () Bool  true)
  (define-fun safe_has_cap               () Bool  true)
)
```

## Reference

http://www.ycombinator.com/docs/SAFE_Primer.rtf

