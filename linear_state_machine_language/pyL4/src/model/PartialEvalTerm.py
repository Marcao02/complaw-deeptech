from src.model.EvalContext import EvalContext
from src.model.Term import Term


class PartialEvalTerm(Term):
    def __init__(self,
                 term:Term,
                 ctx:EvalContext) -> None:
        self.term = term
        self.ctx = ctx
        # self.gvar_subst = gvar_subst
        # self.abap_subst = abap_subst

    def __str__(self) -> str:
        return str(self.term) + f" [{self.ctx}]"