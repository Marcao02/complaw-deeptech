from typing import NamedTuple

from model.Term import Term


class ContractParamDec(NamedTuple):
    name: str
    sort: str
    value_expr: Term

    def __str__(self) -> str:
        return self.name + " : " + self.sort + " := " + str(self.value_expr)
