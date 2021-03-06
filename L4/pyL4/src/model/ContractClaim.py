from typing import Any

from src.model.Term import Term


class ContractClaim:
    # later after handle CONTRACT_VALUE_PROPERTIES
    # def __init__(self, term:Term) -> None:
    #     self.term = term
    # def __str__(self):
    #     return str(self.term)

    def __init__(self, expr:Any) -> None: # currently expr is an SExpr
        self.expr = expr
    def __str__(self):
        return str(self.expr)


class StateInvariant:
    def __init__(self, prop:Term) -> None:
        self.prop = prop
    def __str__(self):
        return str(self.prop)