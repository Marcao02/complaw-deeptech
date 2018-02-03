from typing import List

from src.independent.util import indent
from src.model.Statement import Statement


class GlobalStateTransform:
    def __init__(self, statements: List[Statement]) -> None:
        self.statements = statements

    def __str__(self):
        rv = ""
        def line(s, newindent=0):
            nonlocal rv
            rv += indent(newindent) + s + "\n"

        line("transform:",1)
        for statement in self.statements:
            rv += statement.toStr(2) + "\n"

        # kill the final \n
        rv = rv[:-1]

        return rv

