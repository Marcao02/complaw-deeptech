from src.typesystem.Sorts import *


class SimpleFnType(NamedTuple):
    parts: Tuple[Sort,...]
    @property
    def dom(self) -> Tuple[Sort,...]:
        return self.parts[:-1]
    def ran(self) -> Sort:
        return self.parts[0]

class ArbArityFnType(NamedTuple):
    dom: Sort
    ran: Sort

class OverloadedFnType(NamedTuple):
    parts: Tuple[SimpleFnType,...]

