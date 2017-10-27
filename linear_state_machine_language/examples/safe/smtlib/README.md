# SMTLIB solutions to SAFE and KISS interactions

```
20171027-19:07:24 mengwong@venice2:~/non-db-src/l/compiler/linear_state_machine_language/examples/safe/smtlib% cat safe-pre.z3 safe-example1.z3 safe-post.z3 | z3 -in
sat
(model
  (define-fun kiss_discount () Real      0.0)
  (define-fun kiss_cap () Real        2000.0)
  (define-fun kiss_value () Real       200.0)
  (define-fun safe_discount () Real      0.0)
  (define-fun safe_cap () Real        2000.0)
  (define-fun safe_value () Real       300.0)
  (define-fun initialStock () Real     100.0)
  (define-fun safe_pps () Real         (/ 197.0 11.0))
  (define-fun kiss_pps () Real         (/ 394.0 23.0))
  (define-fun kiss_num_shares () Real  (/ 2300.0 197.0))
  (define-fun safe_num_shares () Real  (/ 3300.0 197.0))
)
```


