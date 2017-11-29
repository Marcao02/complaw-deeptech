from typing import Dict, Any

from model.Term import Term
# from model.constants_and_defined_types import ActionParamSubst
from model.constants_and_defined_types import LocalOrGlobalVarId, GlobalVarId


class PartialEvalTerm(Term):
    def __init__(self, term:Term, subst:Dict[GlobalVarId,Any]) -> None:
        self.term = term
        self.subst = subst

    def __str__(self) -> str:
        return str(self.term) + f" [{self.subst}]"