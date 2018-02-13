from typing import NewType

from src.model.StateTransform import StateTransform
from src.independent.util import mapjoin
from src.parse_to_model.sexpr_to_L4Contract import isprimed, unprimed
from src.constants_and_defined_types import ActionBoundActionParamId, ActionId
from src.model.Sort import Sort
from src.model.StateVarDec import StateVarDec
from src.model.Literal import Literal, SortLit
from src.model.Action import Action
from src.model.BoundVar import GlobalVar, BoundVar, ActionBoundActionParam
from src.model.L4Contract import L4Contract
from src.model.Term import Term, FnApp
from src.independent.typing_imports import *
from src.model.Statement import LocalVarDec, StateVarAssign, IfElse, FVRequirement, Statement
Block = List[Statement]

# Z3Expr = Union[str, int, Tuple[Any, ...]]
# Z3Statement = Tuple[Union[str, Z3Expr], ...]
Z3NonatomicExpr = NewType('Z3NonatomicExpr',Tuple[Any])
Z3Atom = Union[str,int,float,bool]
Z3Expr = Union[Z3NonatomicExpr,Z3Atom]
Z3Statement = NewType('Z3Statement',Tuple[Any])
Z3Line = Union[Z3Statement,str]

Z3_BUILDIN_FNS = frozenset({'and','or','not','=>','=','*','+','>','<','<=','>=','/','-'})

SORT_TO_PRED : Dict[str,Callable[[str],Z3Expr]]= {
    "$": lambda x: fnapp(">=", x, 0),
    "Pos$": lambda x: fnapp(">", x, 0),
    "PosReal": lambda x: fnapp(">", x, 0),
    "ShareCnt": lambda x: fnapp(">=", x, 0),
    "Nat": lambda x: fnapp(">=", x, 0),
    "PosShareCnt": lambda x: fnapp(">", x, 0),
    "SharePrice": lambda x: fnapp(">=", x, 0),
    "Fraction[0,1)": lambda x: conj( fnapp(">=", x, 0), fnapp("<", x, 1) ),
    "Fraction[0,1]": lambda x: conj( fnapp(">=", x, 0), fnapp("<=", x, 1) ),
    "Fraction(0,1]": lambda x: conj( fnapp(">", x, 0), fnapp("<=", x, 1) )
}

FN_NAME_SUBST : Dict[str,str] = {
    'ifthenelse': 'ite',
    '≤' : '<=',
    '≥' : '>=',
    '==' : '=',
}

MACRO_DEFINED_FNS : Dict[str, Callable] = {
    "days": lambda x: x,
    "cast": lambda S,t: t,
    "check": lambda S,t: t,
    "trust": lambda S,t: t,
    'fraction-of-sum': lambda a,b: fnapp('/', a, fnapp('+', a, b)),
    'min': lambda a,b: ite(fnapp('<', a, b), a, b),
     # a round/ b is floor(a/b) + (1 if rem(a,b) >= floor(b/2) else 0)
    'round/': lambda a,b: fnapp('+', fnapp('div', a, b), ite( fnapp('>=', fnapp('rem',a,b), fnapp('div',b,2)), 1, 0)),

    # not used in SAFE:
    # 'ceil': ,
    # 'round': ,
    # 'even': ,
    # 'odd'
    # 'max': ,
}

def disj(*args:Z3Expr) -> Z3NonatomicExpr:
    return cast(Z3NonatomicExpr, ("or", *args))
def conj(*args:Z3Expr) -> Z3NonatomicExpr:
    return cast(Z3NonatomicExpr, ("and", *args))
def neg(arg:Z3Expr) -> Z3NonatomicExpr:
    return cast(Z3NonatomicExpr, ("not", arg))
def implies(arg1:Z3Expr, arg2:Z3Expr) -> Z3NonatomicExpr:
    return cast(Z3NonatomicExpr, ("=>", arg1, arg2))
    # return cast(Z3NonatomicExpr, ("or",("not",arg1), arg2))
def equals(arg1:Z3Expr, arg2:Z3Expr) -> Z3NonatomicExpr:
    return cast(Z3NonatomicExpr, ("=", arg1, arg2))
def ite(a1:Z3Expr, a2:Z3Expr, a3:Z3Expr) -> Z3NonatomicExpr:
    return cast(Z3NonatomicExpr, ('ite',a1,a2,a3))

def fnapp(symb:str, *args:Z3Expr) -> Z3NonatomicExpr:
    return cast(Z3NonatomicExpr, (symb,*args))

def declareconst(const:str, sort:str) -> Z3Statement:
    return cast(Z3Statement,('declare-const', const, sort))
def assertexpr(expr:Z3Expr) -> Z3Statement:
    return cast(Z3Statement,('assert' , expr))


def z3primed(s:str) -> str:
    return s + "_next"
def z3unprimed(s:str) -> str:
    assert s.endswith("_next")
    return s[-5:]
def isz3primed(s:str):
    return s.endswith("_next")

def rename_with_action_scope(actionparam_name:str, actionid:str) -> str:
    return actionparam_name + "_" + actionid



def z3expr_to_str(x:Union[Z3Expr,str]) -> str:
    if isinstance(x,(str,int,float,bool)):
        return str(x)
    else:
        return f"({mapjoin(z3expr_to_str, x, ' ')})"

INDENT_SIZE = 2
def z3statements_to_str(lines:List[Z3Line]) -> str:
    rv = ""
    indent = 0
    for line in lines:
        if line == "(pop)":
            indent -= 1

        if isinstance(line,str):
            rv += f"{indent*INDENT_SIZE*' '}{line}\n"
        else:
            rv += f"{indent*INDENT_SIZE*' '}({mapjoin(z3expr_to_str, line, ' ')})\n"

        if line == "(push)":
            indent += 1
    return rv


def sort2z3primtype(s: Sort) -> str:
    if isinstance(s,str):
        if s in ("$","Pos$","SharePrice",
                 "Fraction[0,1)","Fraction(0,1]","Fraction[0,1]",
                 "PosReal","TimeDelta"):
            return "Real"
        if s in ("ShareCnt", "PosShareCnt", "Nat"):
            return "Int"
        if s == "Bool":
            return "Bool"


    raise NotImplementedError(str(s) + ", " + str(type(s)))

class ToZ3:
    def __init__(self, prog:L4Contract) -> None:
        self.prog = prog

        self.cast_const_ind = 0

        self.cast_const_decs : Dict[str, Z3Statement] = dict()
        self.cast_const_defs: Dict[str, Z3Statement] = dict()
        self.cast_const_to_enclosing_action : Dict[str,str] = dict()

        self.cast_const_negated_type_asserts: Dict[str, Z3Statement] = dict()

        self.stateTransformDefs: Dict[ActionId, Z3Statement] = dict()

        self.curaid: ActionId = cast(ActionId,"")
        # self.stateTransforms: Dict[ActionId, List[Tuple[Z3Line,int]]] = dict()
        self.stateTransforms: Dict[ActionId, List[Z3Line]] = dict()

        self.topZ3Commands: List[Z3Line] = []
        self.actionZ3Commands: Dict[ActionId, List[Z3Line]] = dict()
        self.curindent: int = 0

        self.state_vars_updated_stack: List[List[str]] = []
        self.state_vars_updated: Set[str] = set()

        self.contractParamDecs : Dict[str,Z3Statement] = dict()
        self.contractParamExtraTypeAssertions : Dict[str, Z3Statement] = dict()
        self.contractParamDefns: Dict[str, Z3Statement] = dict()

        # WARNING THIS CONTAINS THE PRIMED VERSIONS TOO
        self.stateVarDecs : Dict[str,Z3Statement] = dict()
        self.stateVarExtraTypeAssertions: Dict[str, Z3Statement] = dict()

        self.invariant_assertions : List[Z3Statement] = []
        self.invariant_conjectures: List[Z3Expr] = []
        # self.claims : List[Z3Statement] = []

        self.actionParamDecs : Dict[ActionId, Dict[str,Z3Statement]] = dict()
        self.actionParamExtraTypeAssertions : Dict[ActionId, Dict[str, Z3Statement]] = dict()

        self.allz3symbs: Set[str] = set()

    def noteStateVarUpdated(self,var:str):
        self.state_vars_updated_stack[-1].append(var)
        self.state_vars_updated.add(var)

    def append(self, s:Z3Line):
        if self.curaid == "":
            self.topZ3Commands.append(s)
        else:
            # self.stateTransforms[self.curaid].append((s,self.curindent))
            self.actionZ3Commands[self.curaid].append(s)
    def push(self):
        # self.append(("(push)",self.curindent))
        self.append("(push)")
        self.state_vars_updated_stack.append([])
        # self.curindent += 1
    def pop(self):
        # self.append(("(pop)",self.curindent))
        self.append("(pop)")
        self.state_vars_updated.difference_update( self.state_vars_updated_stack.pop() )
        # self.curindent -= 1
    def appendAssert(self, expr:Z3Expr):
        self.append(assertexpr(expr))
    def appendConstDec(self, name:str, sort:str):
        self.append(declareconst(name,sort))

    def appendProofOblig(self, expr:Z3Expr, msg:str = ""):
        self.push()
        if msg:
            self.append(f'(echo "{msg}")')
        else:
            self.append(f'(echo "Try to prove {z3expr_to_str(expr)}")')
        self.appendAssert(neg(expr))
        self.append("(check-sat)")
        self.pop()

    def invariantPrimed(self, inv:Z3Expr):
        if isinstance(inv,str):
            if inv in self.stateVarDecs:
                if isz3primed(inv):
                    raise Exception("Can't use primed state variables in an Invariant.")
                else:
                    return z3primed(inv)
            else:
                return inv
        else:
            return tuple(self.invariantPrimed(x) for x in cast(Z3NonatomicExpr,inv))

    def statevarDecs2z3(self):
        for svd in self.prog.global_var_decs.values():
            self.statevarDec2z3(svd)

    def statevarDec2z3(self, svd:StateVarDec):
        self.stateVarDecs[svd.name] = declareconst(svd.name, sort2z3primtype(svd.sort))
        self.stateVarDecs[z3primed(svd.name)] = declareconst(z3primed(svd.name), sort2z3primtype(svd.sort))
        if svd.sort in SORT_TO_PRED and isinstance(svd.sort, str):
            self.stateVarExtraTypeAssertions[svd.name] = assertexpr(SORT_TO_PRED[svd.sort](svd.name))
        else:
            print(f"Skipping extra type prop for state var {svd.name}:{svd.sort}")

    def contractParamDecs2z3(self):
        for cpd in self.prog.contract_params.values():
            self.contractParamDecs[cpd.name] = declareconst(cpd.name, sort2z3primtype(cpd.sort))
            if cpd.sort in SORT_TO_PRED and isinstance(cpd.sort, str):
                self.contractParamExtraTypeAssertions[cpd.name] = assertexpr(SORT_TO_PRED[cpd.sort](cpd.name))
            else:
                print(f"Skipping extra type prop for contract param {cpd.name}:{cpd.sort}")
            if cpd.value_expr is not None:
                self.contractParamDefns[cpd.name] = assertexpr(equals(cpd.name, self.term2z3def(cpd.value_expr)))

    def actionParams2z3(self, action:Action):
        aid = action.action_id
        self.actionParamDecs[aid] = dict()
        self.actionParamExtraTypeAssertions[aid] = dict()
        for apname,apsort in action.param_sorts_by_name.items():
            scoped_apname = rename_with_action_scope(apname, aid)

            self.actionParamDecs[aid][scoped_apname] = declareconst(scoped_apname, sort2z3primtype(apsort))
            self.append(self.actionParamDecs[aid][scoped_apname])
            if apsort in SORT_TO_PRED:
                assert isinstance(apsort,str)
                self.actionParamExtraTypeAssertions[aid][scoped_apname] = assertexpr(SORT_TO_PRED[apsort](scoped_apname))
                self.append(self.actionParamExtraTypeAssertions[aid][scoped_apname])
            else:
                print(f"Skipping extra type prop for action param {scoped_apname}:{apsort}")

    def prog2z3def(self):
        self.contractParamDecs2z3()
        self.statevarDecs2z3()
        self.invariant2z3()

        for a in self.prog.actions_iter():
            self.action2z3(a)

    def invariant2z3(self):
        for invariant in self.prog.state_invariants:
            inv = self.term2z3def(invariant.prop)
            self.invariant_assertions.append(assertexpr(inv))
            self.invariant_conjectures.append(
                implies(
                    inv,
                    self.invariantPrimed(inv)
                )
            )

    def action2z3(self, a:Action):
        self.curaid = a.action_id
        self.actionZ3Commands[a.action_id] = []
        self.push()
        self.actionParams2z3(a)
        if len(a.preconditions) > 0:
            self.append("; PRE begin")
            for pre in a.preconditions:
                self.appendAssert(self.term2z3def(pre))
            self.append("; PRE end")
        self.stateTransform2z3(a.global_state_transform)
        for invcheck in self.invariant_conjectures:
            # invcheck has the form ('=>', e1, e2)
            self.appendProofOblig(invcheck, f"{self.curaid} INV CHECK:" + z3expr_to_str(invcheck[1])) # type:ignore
        self.pop()

    def stateTransform2z3(self, st:Optional[StateTransform]):

        self.block2z3(st.statements if st else [])

    def block2z3(self,block:Block):
        # assert len(block) > 0
        for s in block:
            self.statement2z3(s)
        for var in self.stateVarDecs:
            if var not in self.state_vars_updated and not isz3primed(var):
                self.appendAssert(equals(z3primed(var), var))

    def statement2z3(self, s:Statement):
        if isinstance(s, StateVarAssign):
            assert s.varop == ":="
            self.appendAssert( equals( z3primed(s.varname), self.term2z3def(s.value_expr) ) )
            self.noteStateVarUpdated(s.varname)

        elif isinstance(s, IfElse):
            self.push()
            self.appendAssert(self.term2z3def(s.test))
            self.block2z3(s.true_branch)
            self.pop()

            if s.false_branch is not None:
                self.push()
                self.appendAssert(neg(self.term2z3def(s.test)))
                self.block2z3(s.false_branch)
                self.pop()

        elif isinstance(s, FVRequirement):
            expr = self.term2z3def(s.value_expr)
            self.appendProofOblig(expr, f"STATIC ASSERT: {z3expr_to_str(expr)}")

        else:
            raise NotImplementedError(str(s))

    def new_cast_const(self,actionid:str) -> str:
        self.cast_const_ind += 1
        rv = "castconst" + str(self.cast_const_ind)
        self.cast_const_to_enclosing_action[rv] = actionid
        return rv
    

    def term2z3def(self,t:Term) -> Z3Expr:
        if isinstance(t, FnApp):
            if t.fnsymb_name in Z3_BUILDIN_FNS:
                return fnapp(t.fnsymb_name, *[self.term2z3def(arg) for arg in t.args])

            elif t.fnsymb_name == "cast" or t.fnsymb_name == "check":
                assert isinstance(t.args[0], SortLit)
                cc = self.new_cast_const(self.curaid)
                sort = t.args[0].lit
                primtype = sort2z3primtype(sort)
                value_expr = self.term2z3def(t.args[1])
                self.push()
                self.appendConstDec(cc, primtype)
                self.appendAssert(equals(cc, value_expr))
                proofoblig = SORT_TO_PRED[sort](cc)
                self.appendProofOblig(proofoblig, f"CAST VERIFY: {z3expr_to_str(proofoblig)}")
                self.pop()
                return value_expr

            elif t.fnsymb_name == "units" or t.fnsymb_name == "trust":
                return self.term2z3def(t.args[1])

            elif t.fnsymb_name in FN_NAME_SUBST:
                return fnapp(FN_NAME_SUBST[t.fnsymb_name], *[self.term2z3def(arg) for arg in t.args])

            else:
                assert t.fnsymb_name in MACRO_DEFINED_FNS, t.fnsymb_name + " unhandled"
                return cast(Z3Expr, MACRO_DEFINED_FNS[t.fnsymb_name]( *(self.term2z3def(arg) for arg in t.args) ))

        elif isinstance(t, BoundVar):
            assert not isinstance(t, LocalVarDec)
            if isprimed(t.name):
                return z3primed(unprimed(t.name))

            if isinstance(t, ActionBoundActionParam):
                return rename_with_action_scope(t.name, t.action.action_id)

            return t.name
        elif isinstance(t, Literal):
            assert isinstance(t.lit, (str,int,float,bool))
            return cast(Z3Expr, t.lit)
        else:
            raise NotImplementedError(f"self.term2z3def({str(t)}), arg type {type(t)}, {isinstance(t, Literal)}")
