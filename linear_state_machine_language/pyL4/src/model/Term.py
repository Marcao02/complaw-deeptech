from typing import List, Union, cast

from src.model.FnSymb import FnSymb
from src.constants_and_defined_types import PREFIX_FN_SYMBOLS, INFIX_FN_SYMBOLS, POSTFIX_FN_SYMBOLS, \
    EXEC_ENV_VARIABLES
from src.typesystem.standard_types import fntypes_map


class Term:
    pass

TermOrStr = Union[Term,str]

class FnApp(Term):
    def __init__(self, head:Union[str, FnSymb], args:List[Term]) -> None:
        self.fnsymb : FnSymb
        if isinstance(head, FnSymb):
            self.fnsymb = head
            fnsymb_name = self.fnsymb.name
        else:
            fnsymb_name = head
        ofntype = fntypes_map[fnsymb_name] if fnsymb_name in fntypes_map else None

        if isinstance(head,str):
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