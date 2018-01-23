from datetime import timedelta
from itertools import chain

from src.independent.typing_imports import *

from src.constants_and_defined_types import SUPPORTED_TIMEUNITS
from src.model.Term import Term
from src.model.Sort import Sort
from src.util import todo_once


class Literal(Term):
    def __init__(self) -> None:
        self.lit : Any

    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe: Optional[Iterable[T]] = None) -> Iterable[T]:
        if iteraccum_maybe:
            return chain(iteraccum_maybe,f(self))
        else:
            return f(self)

    def forEach(self, pred: Callable[[Any], bool], f: Callable[[Any], Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        return rviter

    def __repr__(self):
        return str(self)

class FloatLit(Literal):
    def __init__(self,lit:float) -> None:
        super().__init__()
        self.lit = lit

    def __str__(self) -> str:
        return str(self.lit)

class IntLit(Literal):
    def __init__(self, lit:int) -> None:
        super().__init__()
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class BoolLit(Literal):
    def __init__(self, lit:bool) -> None:
        super().__init__()
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class SortLit(Literal):
    def __init__(self, lit:Sort) -> None:
        super().__init__()
        self.lit : Sort = lit

    def __str__(self):
        return str(self.lit)

class DeadlineLit(Literal):
    def __init__(self, lit:str) -> None:
        super().__init__()
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class RoleIdLit(Literal):
    def __init__(self, lit:str) -> None:
        super().__init__()
        self.lit = lit
    def __str__(self):
        return str(self.lit)


class StringLit(Literal):
    def __init__(self, lit:str) -> None:
        super().__init__()
        self.lit = lit
    def __str__(self):
        return "'" + self.lit + "'"

todo_once("Need to require that timedeltas are nonnegative")

class SimpleTimeDeltaLit(Literal):
    """
    REVERSED because this AST node is for literals like 1M or 4D --> Take an L4Contract instead of a string like 'd','h','w' etc to avoid the use of more than one unit in a contract (which is what `timestamp` is for)

    NOTE some of this functionality is duplicated in interpreter.py
    """
    def __init__(self, num:int, unit:str) -> None:
        super().__init__()
        assert unit in SUPPORTED_TIMEUNITS, f'time unit {unit} unsupported'
        self.num = num
        self.unit = unit
        if self.unit == 'd':
            self.timedelta = timedelta(days=self.num)
        elif self.unit == 'h':
            self.timedelta = timedelta(hours=self.num)
        elif self.unit == 'w':
            self.timedelta = timedelta(weeks=self.num)
        elif self.unit == 'm':
            self.timedelta = timedelta(minutes=self.num)
        else:
            assert self.unit == 's'
            self.timedelta = timedelta(seconds=self.num)

    # Are these used???
    def __lt__(self, other: 'SimpleTimeDeltaLit'):
        return self.timedelta < other.timedelta

    def __le__(self, other: 'SimpleTimeDeltaLit'):
        return self.timedelta <= other.timedelta

    def __str__(self) -> str:
        return f"{self.num}{self.unit}"

    def __repr__(self) -> str:
        return f"SimpleTimeDeltaLit({self.num},{self.unit})"
