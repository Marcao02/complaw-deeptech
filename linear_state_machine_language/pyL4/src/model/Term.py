from typing import List, Union, NamedTuple, Optional

from src.constants_and_defined_types import PREFIX_FN_SYMBOLS, INFIX_FN_SYMBOLS, POSTFIX_FN_SYMBOLS, \
    EXEC_ENV_VARIABLES
from typesystem.FnTypes import OverloadedFnType
from typesystem.standard_types import overloaded_fn_types


class Term:
    pass

TermOrStr = Union[Term,str]

class FnSymb(NamedTuple):
    name: str
    type: Optional[OverloadedFnType]


class FnApp(Term):
    def __init__(self, head:str, args:List[Term]) -> None:
        ofntype = overloaded_fn_types[head] if head in overloaded_fn_types else None
        self.fnsymb = FnSymb(head, ofntype)
        self.args = args

    @property
    def head(self) -> str:
        return self.fnsymb.name

    def __str__(self) -> str:
        if self.head in EXEC_ENV_VARIABLES:
            return self.head
        elif self.head in PREFIX_FN_SYMBOLS:
            if self.head == 'not':
                if isinstance(self.args[0],FnApp):
                    return f"¬({self.args[0]})"
                else:
                    return f"¬{self.args[0]}"
            else:
                return f"({self.head} {' '.join([str(x) for x in self.args])})"
        elif self.head in POSTFIX_FN_SYMBOLS:
            return f"({' '.join([str(x) for x in self.args])} {self.head})"
        else:
            assert self.head in INFIX_FN_SYMBOLS and len(self.args) == 2
            # if isinstance(self.args[0],FnApp) or isinstance(self.args[1],FnApp):
            return f"({self.args[0]} {self.head} {self.args[1]})"
            # else:
            #     return f"{self.args[0]} {self.head} {self.args[1]}"
            # return f"({self.args[0]} {self.head} {self.args[1]})"

    def __repr__(self) -> str:
        return str(self)