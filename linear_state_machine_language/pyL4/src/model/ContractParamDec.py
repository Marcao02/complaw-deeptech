from src.constants_and_defined_types import ContractParamId, SortId
from src.model.Term import Term


class ContractParamDec:
    def __init__(self, name: ContractParamId, sort: SortId, value_expr: Term) -> None:
        self.name = name
        self.sort = sort
        self.value_expr = value_expr

    def __str__(self) -> str:
        return self.name + " : " + self.sort + " := " + str(self.value_expr)
