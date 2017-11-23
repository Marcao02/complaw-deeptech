from typing import List, Union

from model.constants_and_defined_types import PREFIX_FN_SYMBOLS, INFIX_FN_SYMBOLS, POSTFIX_FN_SYMBOLS


class Term:
    pass

TermOrStr = Union[Term,str]

class FnApp(Term):
    def __init__(self, head:str, args:List[Term]) -> None:
        self.head = head
        self.args = args

    def __str__(self):
        if self.head in PREFIX_FN_SYMBOLS:
            return f"({self.head} {' '.join([str(x) for x in self.args])})"
        elif self.head in POSTFIX_FN_SYMBOLS:
            return f"({' '.join([str(x) for x in self.args])} {self.head})"
        else:
            assert self.head in INFIX_FN_SYMBOLS and len(self.args) == 2
            return f"({self.args[0]} {self.head} {self.args[1]})"