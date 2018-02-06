from src.independent.typing_imports import *
from src.model.BoundVar import BoundVar
from src.model.Literal import IntLit
from src.model.Sort import Sort
from src.model.Term import Term, FnApp

class MatchVar(BoundVar):
    def __init__(self, _name:str, sort: Sort) -> None:
        super().__init__()
        self.sort = sort
        self._name = _name

    @property
    def name(self) -> str:
        return self._name

def match(term:Term, pattern:Term) -> bool:
    subst : Dict[str,Term] = {}

    def _match(term:Term, pattern:Term) -> bool:
        if isinstance(pattern,MatchVar):
            if pattern.name in subst:
                if subst[pattern.name] == term:
                    return True
                else:
                    print(f"{pattern.name} already matched to {subst[pattern.name]} so can't match to {term}")
                    return False
            else:
                subst[pattern.name] = term
                # print(f"{pattern.name} <-- {term}")
                return True
        elif isinstance(pattern,FnApp):
            if isinstance(term,FnApp):
                return pattern.fnsymb_name == term.fnsymb_name and \
                       len(pattern.args) == len(term.args) and \
                       (all ( _match(term.args[i],pattern.args[i]) for i in range(len(term.args))))
            else:
                print("nope2")
                return False
        else:
            raise Exception(f"{term}, {pattern}")

    return _match(term, pattern)

assert match(
    FnApp('+', [IntLit(1), IntLit(2), IntLit(1)]),
    FnApp('+', [MatchVar('a','Nat'), MatchVar('b','Nat'), MatchVar('a','Nat')])
)

assert not match(
    FnApp('+', [IntLit(1), IntLit(2), IntLit(3)]),
    FnApp('+', [MatchVar('a','Nat'), MatchVar('b','Nat'), MatchVar('a','Nat')])
)
