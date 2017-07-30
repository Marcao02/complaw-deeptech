# L4 Linear State Machine (LSM) Language

Most documentation about the language itself is currently at http://www.cs.toronto.edu/~wehr/st/out/scc-nogoog/index.html?demo=legalese -- will move it here eventually.

"Linear" in this context is like the "linear" in Linear Temporarl Logic. It means single-threaded. This is good for efficiency, and does not actually preclude concurrency in contracts, but for highly concurrent contracts there would be a state space explosion, as you can see from the following example: Suppose a contract requires party 1 to do {a1,a2,a3} in any order, and requires party 2 to do {b1,b2} in any order. In the worst case, if no states can be joined together, we would use about 2^5 = 32 states for that contract. An example of a state is "party 1 has done a1 and a3 only, and party 2 has done b2 only."

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
