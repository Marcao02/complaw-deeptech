from typing import NamedTuple, List

from src.independent.util import chcast
from src.independent.SExpr import SExpr, sexpr_subst_mult_string, SExprOrStr


class L4Macro(NamedTuple):
    macroparams: List[str]
    macrobody: SExpr

    def subst(self, paramvals:List[SExprOrStr]) -> SExpr:
        return chcast(SExpr, sexpr_subst_mult_string(self.macrobody, self.macroparams, paramvals))
