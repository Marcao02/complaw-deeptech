from datetime import timedelta, datetime
from itertools import chain

from src.independent.util import todo_once
from src.constants_and_defined_types import SUPPORTED_TIMEUNITS
from src.independent.FileCoord import FileCoord
from src.independent.typing_imports import *
from src.model.Sort import Sort
from src.model.Term import Term

T = TypeVar('T')

class Literal(Term):
    def __init__(self, lit:Any, coord:Optional[FileCoord] = None) -> None:
        super().__init__(coord)
        self.lit = lit

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

    def __eq__(self, other:Any) -> bool:
        return isinstance(other,Literal) and self.lit == other.lit

class FloatLit(Literal):
    def __init__(self,lit:float, coord:Optional[FileCoord] = None) -> None:
        super().__init__(lit, coord)
        self.lit = lit

    def __str__(self) -> str:
        return str(self.lit)

class IntLit(Literal):
    def __init__(self, lit:int, coord:Optional[FileCoord] = None) -> None:
        super().__init__(lit,coord)
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class BoolLit(Literal):
    def __init__(self, lit:bool, coord:Optional[FileCoord] = None) -> None:
        super().__init__(lit,coord)
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class SortLit(Literal):
    def __init__(self, lit:Sort, coord:Optional[FileCoord] = None) -> None:
        super().__init__(lit,coord)
        self.lit : Sort = lit

    def __str__(self):
        return str(self.lit)

class RoleIdLit(Literal):
    def __init__(self, lit:str, coord:Optional[FileCoord] = None) -> None:
        super().__init__(lit,coord)
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class StringLit(Literal):
    def __init__(self, lit:str, coord:Optional[FileCoord] = None) -> None:
        super().__init__(lit,coord)
        self.lit = lit
    def __str__(self):
        return "'" + self.lit + "'"

todo_once("Need to require that timedeltas are nonnegative")

class DateTimeLit(Literal):
    def __init__(self, lit:datetime, coord:Optional[FileCoord] = None) -> None:
        super().__init__(lit,coord)
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class TimeDeltaLit(Literal):
    def __init__(self, lit:timedelta, coord:Optional[FileCoord] = None) -> None:
        super().__init__(lit,coord)
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class SimpleTimeDeltaLit(TimeDeltaLit):
    """
    REVERSED because this AST node is for literals like 1M or 4D --> Take an L4Contract instead of a string like 'd','h','w' etc to avoid the use of more than one unit in a contract (which is what `timestamp` is for)

    NOTE some of this functionality is duplicated in interpreter.py
    """
    def __init__(self, num:int, unit:str, coord:Optional[FileCoord] = None) -> None:
        assert unit in SUPPORTED_TIMEUNITS, f'time unit {unit} unsupported'
        self.num = num
        self.unit = unit
        td: timedelta
        if self.unit == 'd':
            td = timedelta(days=self.num)
        elif self.unit == 'h':
            td = timedelta(hours=self.num)
        elif self.unit == 'w':
            td = timedelta(weeks=self.num)
        elif self.unit == 'm':
            td = timedelta(minutes=self.num)
        else:
            assert self.unit == 's'
            td = timedelta(seconds=self.num)
        super().__init__(td, coord)

    # Are these used???
    def __lt__(self, other: 'SimpleTimeDeltaLit'):
        return self.lit < other.lit

    def __le__(self, other: 'SimpleTimeDeltaLit'):
        return self.lit <= other.lit

    def __str__(self) -> str:
        return f"{self.num}{self.unit}"

    def __repr__(self) -> str:
        return f"SimpleTimeDeltaLit({self.num},{self.unit})"
