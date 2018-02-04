from itertools import chain

from src.independent.typing_imports import *

from src.independent.FileCoord import FileCoord
from src.constants_and_defined_types import PREFIX_FN_SYMBOLS, INFIX_FN_SYMBOLS, POSTFIX_FN_SYMBOLS, \
    EXEC_ENV_VARIABLES


class Term:
    def __init__(self, coord: Optional[FileCoord] = None) -> None:
        self.coord = coord

    def forEachTerm(self, f: Callable[['Term'], Iterable[T]], iteraccum_maybe: Optional[Iterable[T]] = None) -> Iterable[T]:
        raise NotImplementedError

    def forEach(self, pred: Callable[[Any], bool], f: Callable[[Any], Iterable[T]]) -> Iterable[T]:
        print(f"{self} of type {type(self)} not handled")
        raise NotImplementedError

    def subst(self, var:str, term:'Term') -> 'Term':
        raise NotImplementedError

    def __eq__(self, other: Any) -> bool:
        raise NotImplemented

TermOrStr = Union[Term,str]

class FnApp(Term):
    def __init__(self, head:Union[str], args:List[Term], coord:Optional[FileCoord] = None) -> None:
        super().__init__(coord)
        self.fnsymb_name = head
        self.args = args
        self.coord = coord

    @property
    def head(self) -> str:
        return self.fnsymb_name

    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe: Optional[Iterable[T]] = None) -> Iterable[T]:
        rviter : Iterable[T] = iteraccum_maybe or []
        rviter = chain(rviter, f(self))
        for term in self.args:
            if isinstance(term,str):
                print("This shouldn't happen, a str as an arg to a fn app:", term)
            rviter = term.forEachTerm(f,rviter)
        return rviter

    def forEach(self, pred: Callable[[Any], bool], f: Callable[[Any], Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        for term in self.args:
            rviter = chain(rviter, term.forEach(pred, f))
        return rviter

    def subst(self, var:str, term:'Term') -> 'Term':
        return FnApp(self.fnsymb_name, [arg.subst(var,term) for arg in self.args], self.coord)

    def __eq__(self,other:Any) -> bool:
        return isinstance(other,FnApp) and self.fnsymb_name == other.fnsymb_name and \
               len(self.args) == len(other.args) and all(self.args[i] == other.args[i] for i in range(len(self.args)))

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