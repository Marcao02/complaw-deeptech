from typing import NamedTuple, List

from src.model.SExpr import SExpr, sexpr_subst_mult_string
from src.model.util import chcast


class StringArgMacro(NamedTuple):
    macroparams: List[str]
    macrobody: SExpr

    def subst(self, paramvals:List[str]) -> SExpr:
        return chcast(SExpr, sexpr_subst_mult_string(self.macrobody, self.macroparams, paramvals))
