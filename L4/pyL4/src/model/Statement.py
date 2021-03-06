from itertools import chain

from src.independent.util import indent
from src.constants_and_defined_types import StateVarId, LocalVarId
from src.independent.typing_imports import *
from src.model.StateVarDec import StateVarDec
from src.model.Sort import Sort
from src.model.Term import Term

StatementList = List['Statement']
T = TypeVar('T')

def blocksubstForVar(b:StatementList, var:str, term:Term) -> StatementList:
    return [s.substForVar(var, term) for s in b]

def blocksubstForTerm(b:StatementList, toremove:Term, term:Term) -> StatementList:
    return [s.substForTerm(toremove, term) for s in b]

""" Just the common parent """
class Statement:
    def __init__(self):
        self.orig : Optional[Statement]
        # we will ensure that the following field isn't None for very long after construction
        self.parent_block : StatementList = None # type: ignore
        self.grandparent_ifelse: Optional[IfElse] = None

    def next_statement(self) -> Optional['Statement']:
        if not self.parent_block:
            rv = None
        else:
            i = self.parent_block.index(self)
            if i < len(self.parent_block) - 1:
                rv = self.parent_block[i+1]
            else:
                # assert self.grandparent_ifelse is not None, self.toStr(0)
                if self.grandparent_ifelse is None:
                    rv = None
                else:
                    rv = self.grandparent_ifelse.next_statement()
        # if rv:
        #     print(f"statement at block index {i}" + "\n" + self.toStr(1) + "\nnext statement is\n" + rv.toStr(1))
        # else:
        #     print(f"statement at block index {i}" + "\n" + self.toStr(1) + "\nhas no next statement")
        return rv


    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        raise NotImplementedError
    def forEach(self, pred:Callable[[Any],bool], f:Callable[[Any],Iterable[T]]) -> Iterable[T]:
        raise NotImplementedError
    def findFirstTerm(self, pred:Callable[[Term],bool]) -> Optional[Term]:
        raise NotImplementedError

    """
    var can be a local or state var
    """
    def substForVar(self, var:str, term:Term) -> 'Statement':
        raise NotImplementedError

    def substForTerm(self, toremove:Term, term:Term) -> 'Statement':
        raise NotImplementedError


    def toStr(self, i:int) -> str:
        return indent(i) + str(self)

    def __str__(self) -> str:
        return self.toStr(0)

    def __repr__(self) -> str:
        return str(self)

class IfElse(Statement):
    def __init__(self, test:Term,
                 true_branch:List[Statement],
                 false_branch: Optional[List[Statement]] = None) -> None:
        super().__init__()
        self.test = test
        self.true_branch = true_branch
        self.false_branch = false_branch

    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        rviter : Iterable[T] = iteraccum_maybe or []
        rviter = chain(rviter, self.test.forEachTerm(f,rviter))
        for statement in self.true_branch:
            rviter = chain(rviter, statement.forEachTerm(f,rviter))
        if self.false_branch:
            for statement in self.false_branch:
                rviter = chain(rviter, statement.forEachTerm(f, rviter))
        return rviter

    def forEach(self, pred:Callable[[Any],bool], f:Callable[[Any],Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        rviter = chain(rviter,self.test.forEach(pred,f))
        for statement in self.true_branch:
            rviter = chain(rviter, statement.forEach(pred,f))
        if self.false_branch:
            for statement in self.false_branch:
                rviter = chain(rviter, statement.forEach(pred, f))
        return rviter

    def findFirstTerm(self, pred: Callable[[Term], bool]) -> Optional[Term]:
        in_test = self.test.findFirstTerm(pred)
        if in_test:
            return in_test
        for statement in self.true_branch:
            payload = statement.findFirstTerm(pred)
            if payload:
                return payload
        if self.false_branch:
            for statement in self.false_branch:
                payload = statement.findFirstTerm(pred)
                if payload:
                    return payload
        return None

    def substForVar(self, var: str, term: Term) -> 'IfElse':
        return IfElse(self.test.substForVar(var, term),
                      blocksubstForVar(self.true_branch, var, term),
                      blocksubstForVar(self.false_branch, var, term) if self.false_branch else None)

    def substForTerm(self, toremove: Term, term: Term) -> 'IfElse':
        return IfElse(self.test.substForTerm(toremove,term),
                      blocksubstForTerm(self.true_branch, toremove, term),
                      blocksubstForTerm(self.false_branch, toremove, term) if self.false_branch else None)

    def toStr(self,i:int):
        rv = indent(i) + f"if {self.test}:\n"
        for x in self.true_branch:
            rv += x.toStr(i+1)
        if self.false_branch:
            rv += "\n" + indent(i) + "else:\n"
            for x in self.false_branch:
                rv += x.toStr(i+1) + "\n"
        return rv

class WithOneTermChild(Statement):
    def __init__(self, value_expr: Term) -> None:
        super().__init__()
        self.value_expr = value_expr

    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        return self.value_expr.forEachTerm(f,iteraccum_maybe)

    def forEach(self, pred:Callable[[Any],bool], f:Callable[[Any],Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        rviter = chain(rviter, self.value_expr.forEach(pred,f))
        return rviter

    def findFirstTerm(self, pred: Callable[[Term], bool]) -> Optional[Term]:
        rv = self.value_expr.findFirstTerm(pred)
        return rv if rv else None



class LocalVarDec(WithOneTermChild):
    def __init__(self, varname:LocalVarId, value_expr:Term, sort:Sort) -> None:
        super().__init__(value_expr)
        self.varname : LocalVarId = varname
        self.sort = sort
        self.is_writeout = False

    def substForVar(self, var:str, term:Term) -> 'LocalVarDec':
        return LocalVarDec(self.varname, self.value_expr.substForVar(var, term), self.sort)

    def substForTerm(self, toremove: Term, term: Term) -> 'LocalVarDec':
        return LocalVarDec(self.varname, self.value_expr.substForTerm(toremove, term), self.sort)

    def __str__(self):
        return f"{self.varname} : {str(self.sort)} := {str(self.value_expr)}"

class FVRequirement(WithOneTermChild):
    def __init__(self, prop:Term) -> None:
        super().__init__(prop)

    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        return self.value_expr.forEachTerm(f, iteraccum_maybe)

    def forEach(self, pred:Callable[[Any],bool], f:Callable[[Any],Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        rviter = chain(rviter, self.value_expr.forEach(pred,f))
        return rviter

    def substForVar(self, var:str, term:Term) -> 'FVRequirement':
        return FVRequirement(self.value_expr.substForVar(var, term))

    def substForTerm(self, toremove: Term, term: Term) -> 'FVRequirement':
        return FVRequirement(self.value_expr.substForTerm(toremove, term))

    def __str__(self) -> str:
        return "prove " + str(self.value_expr)


class StateVarAssign(WithOneTermChild):
    def __init__(self, vardec:StateVarDec, value_expr:Term, varop:str = ":=") -> None:
        super().__init__(value_expr)
        self.vardec = vardec
        self.varop = varop

    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        return self.value_expr.forEachTerm(f, iteraccum_maybe)

    def forEach(self, pred:Callable[[Any],bool], f:Callable[[Any],Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        rviter = chain(rviter, self.value_expr.forEach(pred,f))
        return rviter

    def substForVar(self, var:str, term:Term) -> 'StateVarAssign':
        return StateVarAssign(self.vardec, self.value_expr.substForVar(var, term), self.varop)

    def substForTerm(self, toremove:Term, term:Term) -> 'StateVarAssign':
        return StateVarAssign(self.vardec, self.value_expr.substForTerm(toremove, term), self.varop)


    @property
    def varname(self) -> StateVarId:
        return self.vardec.name

    def __str__(self):
        return f"{self.varname} {self.varop} {str(self.value_expr)}"

