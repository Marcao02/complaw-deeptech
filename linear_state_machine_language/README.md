# L4 Linear State Machine (LSM) Language

Read [LSM Tutorial](https://github.com/legalese/legalese-compiler/blob/master/linear_state_machine_language/LSM_tutorial.md) first.

Most of the [example contracts](https://github.com/legalese/legalese-compiler/tree/master/linear_state_machine_language/examples) are problems from Tom Hvitved's PhD thesis. He wrote executable contracts in the language CSL that he developed. CSL influenced L4/LSM, but in the end they use quite different approaches. 

## Dependencies
You'll need to install Python 3.6 because the code uses format strings e.g. `f"text {code} text"`. It will be installed alongside any other python versions you have, so don't worry about it breaking anything. Also, some version of Python 3 is needed to recognize the static typing annotations. 

https://www.python.org/downloads/release/python-362/

### For generating diagrams
Install graphviz (AKA 'dot'):

http://www.graphviz.org/Download.php

### For typechecking
python3.6 -m pip install -U mypy

mypy compileLSM.py

## Running
python3.6 compileLSM examplesd

## Examples
Todo: SAFE and possibly more examples from Hvitved's thesis.
