from datetime import timedelta

from src.model.Term import Term
from src.model.constants_and_defined_types import SUPPORTED_TIMEUNITS


class SimpleTimeDelta(Term):
    def __init__(self, num:int, unit:str) -> None:
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

    def __lt__(self, other:'SimpleTimeDelta'):
        return self.timedelta < other.timedelta

    def __le__(self, other:'SimpleTimeDelta'):
        return self.timedelta <= other.timedelta

    def __str__(self) -> str:
        return f"{self.num}{self.unit}"

    def __repr__(self) -> str:
        return f"SimpleTimeDelta({self.num},{self.unit})"

# TimeStamp = SimpleTimeDelta
TimeStamp = timedelta

class SimpleDateTime:
    """A datetime rounded to one of the supported time units"""
    def __init__(self):
        pass