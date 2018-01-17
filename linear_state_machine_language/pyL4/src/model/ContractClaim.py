from compiler.SExpr import SExpr

class ContractClaim:
    # later after handle CONTRACT_VALUE_PROPERTIES
    # def __init__(self, term:Term) -> None:
    #     self.term = term
    # def __str__(self):
    #     return str(self.term)
    def __init__(self, expr:SExpr) -> None:
        self.expr = expr
    def __str__(self):
        return str(self.expr)


