from typing import Optional

from src.model.Term import Term
from src.typesystem.Sorts import Sort


class L4TypeError(Exception):
    def __init__(self, msg:str, term:Optional[Term] = None) -> None:
        self.term = term
        self.msg = msg
    def __str__(self) -> str:
        rv = self.msg
        if self.term and self.term.coord:
            if self.term.coord.col:
                rv += f"\nSee (near) line {self.term.coord.line} col {self.term.coord.col}"
            else:
                rv += f"\nSee (near) line {self.term.coord.line}."
        return rv

class L4TypeCheckError(L4TypeError):
    def __init__(self, term:Term, check_sort:Sort) -> None:
        super().__init__(f"Term {term} failed to check against {check_sort}.", term)
        self.check_sort = check_sort

class L4TypeInferError(L4TypeError):
    def __init__(self, term:Term, msg:str="") -> None:
        super().__init__((msg + "\n" if msg else "") + f"Failed to infer sort of term {term}.", term)

class L4TypeInferCheckError(L4TypeError):
    def __init__(self, term:Term, inferred_sort:Sort, check_sort:Sort) -> None:
        super().__init__(f"Term {term}'s inferred sort {inferred_sort} is not a subtype of the sort {check_sort} checked against.", term)
        self.inferred_sort = inferred_sort
        self.check_sort = check_sort
