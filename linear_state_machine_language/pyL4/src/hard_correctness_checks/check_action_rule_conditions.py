"""
unambiguous strong obligation condition: For every reachable global state g, if the guard of some must-next action
rule in stateclass(g) evaluates to true at g, then every other must-next action rule guard in section(g) evaluates
to false at g.


Whenever a contract is running, one of the following is true:
(1). It's waiting on exactly one party to do some particular action, and nobody else can do anything (that the contract
governs) until that happens.
(2). One or more parties have a weaker kind of duty, which gives them possibly-several actions to choose from. If
it's only one party, and only one action, and there are no other permitted actions by any other party, then this is
equivalent to (1). If there is a set of parties P each of whom fail to perform any of their duties
    (a) before those duties' deadlines expire, and
    (b) before any other party does any other permitted action
then P jointly breach the contract.
"""