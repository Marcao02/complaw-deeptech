from typing import NamedTuple, List

from compiler.SExpr import SExpr, sexpr_subst_mult_string, SExprOrStr
from src.util import chcast


class L4Macro(NamedTuple):
    macroparams: List[str]
    macrobody: SExpr

    def subst(self, paramvals:List[SExprOrStr]) -> SExpr:
        return chcast(SExpr, sexpr_subst_mult_string(self.macrobody, self.macroparams, paramvals))
