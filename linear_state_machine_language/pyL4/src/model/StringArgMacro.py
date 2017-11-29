from typing import NamedTuple

from src.model.SExpr import SExpr
from src.model.SExpr import sexpr_subst_string
from src.model.util import chcast


class StringArgMacro(NamedTuple):
    macroparam: str
    macrobody: SExpr

    def subst(self, paramval:str) -> SExpr:
        return chcast(SExpr, sexpr_subst_string(self.macrobody, self.macroparam, paramval))
