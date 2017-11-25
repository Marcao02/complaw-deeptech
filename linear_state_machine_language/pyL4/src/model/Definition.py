from typing import NamedTuple

from model.SExpr import SExpr


class Definition(NamedTuple):
    id: str
    body: SExpr
