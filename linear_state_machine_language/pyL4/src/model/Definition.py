from typing import NamedTuple

from compiler.SExpr import SExpr


class Definition(NamedTuple):
    id: str
    body: SExpr
