

There are many kinds of names in pyL4. You can see them all in `src/model/constants_and_defined_types.py`. Most of them, thankfully, are names for categories of constants: that is, the thing that the name refers to does not vary during execution or formal verification of L4 contracts.

This document will not focus on constant-like names, but the others, the variable-like names.

First, I'll dismiss with one category that's at the boundary between constant-like and variable-like: contract parameters (`ContractParamDec` for declaration and `ContractParam` for usages).

`ActionBoundActionParamId`

`RuleBoundActionParamId`

