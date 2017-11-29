from typing import Any
from src.model.Term import Term


class Literal(Term):
    def __init__(self) -> None:
        self.lit : Any
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
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class BoolLit(Literal):
    def __init__(self, lit:bool) -> None:
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class DeadlineLit(Literal):
    def __init__(self, lit:str) -> None:
        self.lit = lit
    def __str__(self):
        return str(self.lit)

class StringLit(Literal):
    def __init__(self, lit:str) -> None:
        self.lit = lit
    def __str__(self):
        return "'" + self.lit + "'"
