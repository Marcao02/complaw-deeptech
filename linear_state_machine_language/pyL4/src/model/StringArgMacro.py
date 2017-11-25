from typing import NamedTuple

from model.SExpr import SExpr
from model.SExpr import sexpr_subst_string
from model.util import chcast


class StringArgMacro(NamedTuple):
    macroparam: str
    macrobody: SExpr

    def subst(self, paramval:str) -> SExpr:
        return chcast(SExpr, sexpr_subst_string(self.macrobody, self.macroparam, paramval))
