from typing import NamedTuple

from model.Term import Term
from model.constants_and_defined_types import ContractParamId, SortId


class ContractParamDec(NamedTuple):
    name: ContractParamId
    sort: SortId
    value_expr: Term

    def __str__(self) -> str:
        return self.name + " : " + self.sort + " := " + str(self.value_expr)
