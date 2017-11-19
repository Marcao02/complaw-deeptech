# pyL4 (Linear State Machine based)

Out-of-date [Tutorial](https://github.com/legalese/legalese-compiler/blob/master/linear_state_machine_language/LSM2_tutorial.md).

<!-- Most of the [example contracts](https://github.com/legalese/legalese-compiler/tree/master/linear_state_machine_language/examplesLSM2) are problems from Tom Hvitved's PhD thesis. He wrote executable contracts in the language CSL that he developed. CSL influenced L4/LSM, but in the end they use quite different approaches.  -->

## Dependencies
You'll need to install Python 3.6 because the code uses format strings e.g. `f"text {code} text"` and Named Tuple syntax. It will be installed alongside any other python versions you have, so don't worry about it breaking anything. Also, some version of Python 3 is needed to recognize the static typing annotations. 

https://www.python.org/downloads/release/python-362/

### For generating diagrams
Install graphviz (AKA 'dot'):

http://www.graphviz.org/Download.php

### For typechecking
Install mypy with 

`python3.6 -m pip install -U mypy`

To typecheck, from `pyL4` do `mypy src`.

## Running
`python3.6 src/cli.py examples printPretty printSExpr dot`

## Before committing 
`python3.6 src/before_commit.py` simply runs the previous two commands

## Relation to Linear State Machines

`should` in L4 is, for the sake of execution, a synonym of `may`. We will differentiate between the two in other situations though, e.g. to define the "happy paths".

About leaving out `(TransitionsTo ‹section_id›`...

**Todo: update LaTeX to add "env action" as a connection type.**

<!--In LSM, there is only one type of `Connection`, which has a role, action, and a few other things.-->
<!--In L4, there are ConnectionToAction, ConnectionToEnvAction. They are just conveniences:-->

<!--* ConnectionToSection has no `deontic_modality` but has an extra `dest_id` (a `Section` id), its `role_id` is always `ENV_ROLE`, and its `action_id` is its `dest_id` prefixed with `Enter`.-->
<!--* ConnectionToEnvAction has no `deontic_modality` and its `role_id` is always `ENV_ROLE`.-->

