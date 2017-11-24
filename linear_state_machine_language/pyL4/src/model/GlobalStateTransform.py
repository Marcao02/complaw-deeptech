from typing import List

from model.GlobalStateTransformStatement import GlobalStateTransformStatement


class GlobalStateTransform:
    def __init__(self, statements: List[GlobalStateTransformStatement]) -> None:
        self.statements = statements

    def __str__(self):
        rv = ""
        def line(s, newindent=0):
            nonlocal rv
            rv += 4*newindent*" " + str(s) + "\n"

        line("transform:",1)
        for statement in self.statements:
            line(statement, 2)

        # kill the final \n
        rv = rv[:-1]

        return rv

