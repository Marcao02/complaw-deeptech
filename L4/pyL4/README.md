# pyL4 (the only L4 implementation)

<!--Some limitted info about [L4 readme.md](https://github.com/legalese/complaw-deeptech/blob/master/linear_state_machine_language/README.md)-->

<!-- Most of the [example contracts](https://github.com/legalese/complaw-deeptech/tree/master/linear_state_machine_language/examplesLSM2) are problems from Tom Hvitved's PhD thesis. He wrote executable contracts in the language CSL that he developed. CSL influenced L4/LSM, but in the end they use quite different approaches.  -->

## Dependencies
You'll need to install Python 3.6 because the code uses format strings e.g. `f"text {code} text"` and Named Tuple syntax. It will be installed alongside any other python versions you have, so don't worry about it breaking anything. Some version of Python 3 is also needed to recognize the static typing annotations.

https://www.python.org/downloads/release/python-362/

### (Optional) SMT Solver

`pip3 install z3` or maybe `python3.6 -m pip install z3`

### (Optional) For generating contract diagrams
Install graphviz (AKA 'dot'):

http://www.graphviz.org/Download.php

### (Optional) For natural language generation to HTML

`pip3 install dominate`

### (Optional) For typechecking the python code
Install [mypy](http://mypy-lang.org/) with

`python3.6 -m pip install -U mypy`

To typecheck, from `pyL4` do `python3.6 run_me_before_commit.py tconly`.



## Running

You can control which modules to run examples on, and which examples to run, with `tests_to_run` in `runme_before_commit.py` and by commenting out lines in `ALL_EXAMPLES` in `test/active_examples.py`.

`python3.6 runme_before_commit.py`

`python3.6 runme_before_commit.py tconly`

<!--`python3.6 test/test_smt.py`

`python3.6 test/test_interpreter.py`

`python3.6 test/test_typechecker.py`

`python3.6 test/test_prettyprint.py`

`python3.6 test/test_graphviz.py`
-->
<!--`python3.6 test/test_parser.py` (this file gets used by all the other `test_`* files, as well as `run_me_before_commit.py`)
-->

## Relation to Linear State Machines

**This section requires a lot more work**

<!--`should` in L4 is, for the sake of execution, a synonym of `may`. We will differentiate between the two in other situations though, e.g. to define the "happy paths".
-->
<!--About leaving out `(TransitionsTo ‹situation_id› ...)` and that kind of thing.-->

<!-- **Todo: update LaTeX to add "env action" as an action_rule type.** -->

<!--In LSM, there is only one type of `NextActionRule`, which has a role, action, and a few other things.-->
<!--In L4, there are PartyNextActionRule, EnvNextActionRule. They are just conveniences:-->

<!--* ActionRuleToSituation has no `deontic_keyword` but has an extra `dest_id` (a `Situation` id), its `role_id` is always `ENV_ROLE`, and its `action_id` is its `dest_id` prefixed with `Enter`.-->
<!--* EnvNextActionRule has no `deontic_keyword` and its `role_id` is always `ENV_ROLE`.-->

