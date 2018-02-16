from src.independent.typing_imports import *
from src.model.BoundVar import BoundVar
from src.model.Literal import IntLit
from src.model.Sort import Sort, SortOpApp
from src.model.Term import Term, FnApp

# class TermMatchVar(BoundVar):
#     def __init__(self, _name:str, type: Sort) -> None:
#         super().__init__()
#         self.type = Sort
#         self._name = _name
#
#     @property
#     def name(self) -> str:
#         return self._name
class TermMatchVar(NamedTuple):
    name: str
    vartype: Sort

def matchTerm(concrete_term:Term, term_pattern:Term) -> bool:
    subst : Dict[str,Term] = {}

    def match(term:Term, pat:Term) -> bool:
        if isinstance(pat, TermMatchVar):
            varname: str = pat.name # type:ignore
            if varname in subst:
                if subst[varname] == term:
                    return True
                else:
                    print(f"{varname} already matched to {subst[varname]} so can't matchTerm to {term}")
                    return False
            else:
                subst[varname] = term
                # print(f"{pattern.name} <-- {term}")
                return True
        elif isinstance(pat, FnApp):
            if isinstance(term,FnApp):
                return pat.fnsymb_name == term.fnsymb_name and \
                       len(pat.args) == len(term.args) and \
                       (all (match(term.args[i], pat.args[i]) for i in range(len(term.args))))
            else:
                print("nope2")
                return False
        else:
            raise Exception(f"{term}, {pat}")

    return match(concrete_term, term_pattern)


class SortMatchVar(NamedTuple):
    name: str
    vartype: str # should be 'UnitType' or 'Sort'

# stub
def matchSort(concrete_sort:Sort, sort_pattern:Sort, start_subst: Optional[Dict[str,Sort]] = None) -> Dict[str,Sort]:
    subst: Dict[str, Sort] = start_subst or {}

    def match(sort:Sort, pat:Sort) -> bool:
        if isinstance(pat, SortMatchVar):
            varname : str = pat.name # type:ignore
            if varname in subst:
                if subst[varname] == sort:
                    return True
                else:
                    print(f"Match var {varname} already matched to {subst[varname]} so can't match to {sort}")
                    return False
            else:
                subst[varname] = sort
                # print(f"{pattern.name} <-- {term}")
                return True

        elif isinstance(pat, SortOpApp):
            if isinstance(sort, SortOpApp):
                return pat.op == sort.op  and \
                       len(pat.args) == len(sort.args) and \
                       (all (match(sort.args[i], pat.args[i]) for i in range(len(sort.args))))
            else:
                return False
        else:
            raise Exception(f"{sort}, {pat}")

    match(concrete_sort, sort_pattern)
    return subst


print("testing matchTerm...")
assert matchTerm(
    FnApp('+', [IntLit(1), IntLit(2), IntLit(1)]),
    FnApp('+', [TermMatchVar('a','Nat'), TermMatchVar('b','Nat'), TermMatchVar('a','Nat')]) # type:ignore
)

assert not matchTerm(
    FnApp('+', [IntLit(1), IntLit(2), IntLit(3)]),
    FnApp('+', [TermMatchVar('a','Nat'), TermMatchVar('b','Nat'), TermMatchVar('a','Nat')]) # type:ignore
)
