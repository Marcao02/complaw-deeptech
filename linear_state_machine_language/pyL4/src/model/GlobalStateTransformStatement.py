from itertools import chain

from src.independent.util import indent
from src.constants_and_defined_types import GlobalVarId, StateTransformLocalVarId
from src.independent.typing_imports import *
from src.model.GlobalVarDec import GlobalVarDec
from src.model.Sort import Sort
from src.model.Term import Term

Block = List['GlobalStateTransformStatement']

def blocksubst(b:Block, var:str, term:Term) -> Block:
    return [s.subst(var,term) for s in b]

""" Just the common parent """
class GlobalStateTransformStatement:
    def __init__(self):
        self.orig : Optional[GlobalStateTransformStatement]

    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        raise NotImplementedError
    def forEach(self, pred:Callable[[Any],bool], f:Callable[[Any],Iterable[T]]) -> Iterable[T]:
        raise NotImplementedError

    """
    var can be a local or global var
    """
    def subst(self, var:str, term:Term) -> 'GlobalStateTransformStatement':
        raise NotImplementedError

    def toStr(self, i:int):
        return indent(i) + str(self)


class IfElse(GlobalStateTransformStatement):
    def __init__(self, test:Term,
                 true_branch:List[GlobalStateTransformStatement],
                 false_branch: Optional[List[GlobalStateTransformStatement]] = None) -> None:
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

    def subst(self, var: str, term: Term) -> 'IfElse':
        return IfElse( self.test.subst(var,term),
                       blocksubst(self.true_branch, var, term),
                       blocksubst(self.false_branch, var, term) if self.false_branch else None )

    def toStr(self,i:int):
        rv = indent(i) + f"if {self.test}:\n"
        for x in self.true_branch:
            rv += indent(i + 1) + str(x) + "\n"
        if self.false_branch:
            rv += indent(i) + "else:\n"
            for x in self.false_branch:
                rv += indent(i + 1) + str(x) + "\n"
        return rv


class StateTransformLocalVarDec(GlobalStateTransformStatement):
    def __init__(self, varname:StateTransformLocalVarId, value_expr:Term, sort:Sort) -> None:
        super().__init__()
        self.varname : StateTransformLocalVarId = varname
        self.value_expr = value_expr
        self.sort = sort

    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        return self.value_expr.forEachTerm(f,iteraccum_maybe)

    def forEach(self, pred:Callable[[Any],bool], f:Callable[[Any],Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        rviter = chain(rviter, self.value_expr.forEach(pred,f))
        return rviter

    def __str__(self):
        return f"{self.varname} : {str(self.sort)} := {str(self.value_expr)}"

# class LocalVarDec(GlobalVarAssignStatement):
#     def __init__(self, varname:LocalVarId, value_expr:Term, sort:str) -> None:
#         super().__init__(castid(GlobalVarId,varname), value_expr)
#         self.sort = sort
#
#     def __str__(self):
#         return f"{self.varname} := {str(self.value_expr)}"

class InCodeConjectureStatement(GlobalStateTransformStatement):
    def __init__(self, prop:Term) -> None:
        super().__init__()
        self.value_expr = prop

    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        return self.value_expr.forEachTerm(f, iteraccum_maybe)

    def forEach(self, pred:Callable[[Any],bool], f:Callable[[Any],Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        rviter = chain(rviter, self.value_expr.forEach(pred,f))
        return rviter


    def __str__(self) -> str:
        return "prove " + str(self.value_expr)


class AbstractGlobalVarAssignStatement(GlobalStateTransformStatement):
    def __init__(self, vardec:GlobalVarDec, value_expr:Term) -> None:
        super().__init__()
        self.vardec = vardec
        self.value_expr = value_expr

    def forEachTerm(self, f: Callable[[Term], Iterable[T]], iteraccum_maybe:Optional[Iterable[T]] = None) -> Iterable[T]:
        return self.value_expr.forEachTerm(f, iteraccum_maybe)

    def forEach(self, pred:Callable[[Any],bool], f:Callable[[Any],Iterable[T]]) -> Iterable[T]:
        rviter: Iterable[T] = []
        if pred(self):
            rviter = chain(rviter, f(self))
        rviter = chain(rviter, self.value_expr.forEach(pred,f))
        return rviter



    @property
    def varname(self) -> GlobalVarId:
        # this cast shouldn't be necessary. weird.
        # return cast(GlobalVarId,self.vardec.name)
        return self.vardec.name

class GlobalVarAssignStatement(AbstractGlobalVarAssignStatement):
    def __str__(self):
        return f"{self.varname} := {str(self.value_expr)}"
class IncrementStatement(AbstractGlobalVarAssignStatement):
    def __str__(self):
        return f"{self.varname} += {str(self.value_expr)}"
class DecrementStatement(AbstractGlobalVarAssignStatement):
    def __str__(self):
        return f"{self.varname} -= {str(self.value_expr)}"
class TimesEqualsStatement(AbstractGlobalVarAssignStatement):
    def __str__(self):
        return f"{self.varname} *= {str(self.value_expr)}"
