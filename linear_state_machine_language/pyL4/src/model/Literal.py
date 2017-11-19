from model.Term import Term


class Literal(Term):
    def __repr__(self):
        return str(self)

class FloatLit(Literal):
    def __init__(self,val:float) -> None:
        super().__init__()
        self.val = val

    def __str__(self) -> str:
        return str(self.val)

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
