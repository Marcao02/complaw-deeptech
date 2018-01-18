from typing import NamedTuple

from src.compiler.SExpr import SExpr


class Definition(NamedTuple):
    id: str
    body: SExpr
