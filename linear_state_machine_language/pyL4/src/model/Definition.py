from typing import NamedTuple

from src.model.SExpr import SExpr


class Definition(NamedTuple):
    id: str
    body: SExpr
