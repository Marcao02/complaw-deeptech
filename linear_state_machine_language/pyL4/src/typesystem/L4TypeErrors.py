
from src.model.Term import Term
from src.typesystem.Sorts import Sort


class L4TypeError(Exception):
    pass


class L4TypeCheckError(Exception):
    def __init__(self, term:Term, check_sort:Sort) -> None:
        self.term = term
        self.check_sort = check_sort
    def __str__(self) -> str:
        return f"Term {self.term} failed to check against {self.check_sort}"

class L4TypeInferError(Exception):
    def __init__(self, term:Term) -> None:
        self.term = term
    def __str__(self) -> str:
        return f"Failed to infer sort of term {self.term}."

class L4TypeInferCheckError(Exception):
    def __init__(self, term:Term, inferred_sort:Sort, check_sort:Sort) -> None:
        self.term = term
        self.inferred_sort = inferred_sort
        self.check_sort = check_sort
    def __str__(self) -> str:
        return f"Term {self.term}'s inferred sort {self.inferred_sort} is not a subtype of the sort {self.check_sort} checked against."
