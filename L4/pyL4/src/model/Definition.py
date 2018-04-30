from typing import NamedTuple

from src.independent.SExpr import SExpr


class Definition(NamedTuple):
    id: str
    body: SExpr
