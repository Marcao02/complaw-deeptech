from typing import NamedTuple

from model.SExpr import SExpr
from model.SExpr import sexpr_subst_string


class StringArgMacro(NamedTuple):
    macroparam: str
    macrobody: SExpr

    def subst(self, paramval:str):
        return sexpr_subst_string(self.macrobody, self.macroparam, paramval)
