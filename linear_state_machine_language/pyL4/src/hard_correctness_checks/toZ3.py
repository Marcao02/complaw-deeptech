from typing import NewType

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
Z3IndentedLinesList = List[Tuple[Z3Line, int]]

Z3_BUILDIN_FNS = frozenset({'and','or','not','==','*','+','>','<','<=','>=','/','-'})

SORT_TO_PRED : Dict[str,Callable[[str],Z3Expr]]= {
    "$": lambda x: fnapp(">=", x, 0),
    "Pos$": lambda x: fnapp(">", x, 0),
    "ShareCnt": lambda x: fnapp(">=", x, 0),
    "PosShareCnt": lambda x: fnapp(">", x, 0),
    "SharePrice": lambda x: fnapp(">=", x, 0),
    "Fraction[0,1)": lambda x: conj( fnapp(">=", x, 0), fnapp("<", x, 1) ),
    "Fraction(0,1]": lambda x: conj( fnapp(">", x, 0), fnapp("<=", x, 1) )
}

def disj(*args:Z3Expr) -> Z3NonatomicExpr:
    return cast(Z3NonatomicExpr, ("or", *args))
def conj(*args:Z3Expr) -> Z3NonatomicExpr:
    return cast(Z3NonatomicExpr, ("and", *args))
def neg(arg:Z3Expr) -> Z3NonatomicExpr:
    return cast(Z3NonatomicExpr, ("not", arg))
def implies(arg1:Z3Expr, arg2:Z3Expr) -> Z3NonatomicExpr:
    return cast(Z3NonatomicExpr, ("or",("not",arg1), arg2))
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

def rename_with_action_scope(actionparam_name:str, actionid:str) -> str:
    return actionparam_name + "_" + actionid



def z3expr_to_str(x:Union[Z3Expr,str]) -> str:
    if isinstance(x,(str,int,float,bool)):
        return str(x)
    else:
        return f"({mapjoin(z3expr_to_str, x, ' ')})"

def z3statements_to_str(items:Z3IndentedLinesList) -> str:
    rv = ""
    for item in items:
        (printable, indent) = item
        if isinstance(printable,str):
            rv += f"{indent*'    '}{printable}\n"
        else:
            rv += f"{indent*'    '}({mapjoin(z3expr_to_str, printable, ' ')})\n"
    return rv

FN_NAME_SUBST : Dict[str,str] = {
    'ifthenelse': 'ite',
    '≤' : '<=',
    '≥' : '>=',
}

MACRO_DEFINED_FNS : Dict[str, Callable] = {
    'cast': lambda S,t: t,
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


def sort2z3primtype(s: Sort) -> str:
    if isinstance(s,str):
        if s in ("$","Pos$","SharePrice","Fraction[0,1)","Fraction(0,1]"):
            return "Real"
        if s in ("ShareCnt", "PosShareCnt"):
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

        self.contractParamDecs : Dict[str,Z3Statement] = dict()
        self.contractParamExtraTypeAssertions : Dict[str, Z3Statement] = dict()
        self.contractParamDefns: Dict[str, Z3Statement] = dict()

        self.stateVarDecs : Dict[str,Z3Statement] = dict()
        self.stateVarExtraTypeAssertions: Dict[str, Z3Statement] = dict()

        self.invariants : List[Z3Statement] = []
        self.claims : List[Z3Statement] = []

        self.actionParamDecs : Dict[ActionId, Dict[str,Z3Statement]] = dict()
        self.actionParamExtraTypeAssertions : Dict[ActionId, Dict[str, Z3Statement]] = dict()

        self.allz3symbs: Set[str] = set()

        self.curaid : str = ""


    def statevarDec2z3(self, svd:StateVarDec):
        self.stateVarDecs[svd.name] = declareconst(svd.name, sort2z3primtype(svd.sort))
        self.stateVarDecs[z3primed(svd.name)] = declareconst(z3primed(svd.name), sort2z3primtype(svd.sort))
        if svd.sort in SORT_TO_PRED and isinstance(svd.sort, str):
            self.stateVarExtraTypeAssertions[svd.name] = assertexpr(SORT_TO_PRED[svd.sort](svd.name))
            # self.stateVarDecsExtra[primed(svd.name)] = assertexpr(SORT_TO_PRED[svd.sort](primed(svd.name)))
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
            if apsort in SORT_TO_PRED:
                assert isinstance(apsort,str)
                self.actionParamExtraTypeAssertions[aid][scoped_apname] = assertexpr(SORT_TO_PRED[apsort](scoped_apname))
            else:
                print(f"Skipping extra type prop for action param {scoped_apname}:{apsort}")

    def prog2z3def(self, prog:L4Contract):
        self.contractParamDecs2z3()

        for svd in self.prog.global_var_decs.values():
            self.statevarDec2z3(svd)

        for invariant in self.prog.state_invariants:
            self.invariants.append(assertexpr(self.term2z3def(invariant.prop)))

        for a in self.prog.actions_iter():
            self.curaid = a.action_id
            self.actionParams2z3(a)
            self.action2z3def(a)

    def action2z3def(self, a:Action):
        if a.global_state_transform:
            self.stateTransformDefs[a.action_id] = assertexpr(self.block2z3def(a.global_state_transform.statements))
    
    def block2z3def(self,block:Block) -> Z3Expr:
        assert len(block) > 0
        if len(block) == 1:
            return self.statement2z3def(block[0])
        else:
            return conj(*(self.statement2z3def(s) for s in block))
    
    def statement2z3def(self,s:Statement) -> Z3Expr:
        if isinstance(s,StateVarAssign):
            assert s.varop == ":="
            return equals(z3primed(s.varname), self.term2z3def(s.value_expr))
        elif isinstance(s,IfElse):
            if s.false_branch:
                return conj(
                    implies(self.term2z3def(s.test),      self.block2z3def(s.true_branch)),
                    implies(neg(self.term2z3def(s.test)), self.block2z3def(s.false_branch))
                )
            else:
                return implies(self.term2z3def(s.test),   self.block2z3def(s.true_branch))
        elif isinstance(s, FVRequirement):
            return "true"
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
            elif t.fnsymb_name == "cast":
                cc = self.new_cast_const(self.curaid)
                assert isinstance(t.args[0], SortLit)
                sort = t.args[0].lit
                primtype = sort2z3primtype(sort)
                self.cast_const_decs[cc] = declareconst(cc,primtype)
                value_expr = self.term2z3def(t.args[1])
                self.cast_const_defs[cc] = assertexpr(
                   equals(cc, value_expr)
                )
                self.cast_const_negated_type_asserts[cc] = assertexpr(
                    neg( SORT_TO_PRED[sort](cc) )
                )
                return value_expr
            elif t.fnsymb_name == "units":
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
    
    
    def block_requirements_to_z3(self,hyp:List[Z3Expr], block:Block) -> Z3Expr:
        assert len(block) > 0
        if len(block) == 1:
            return self.statement_requirements_to_z3(hyp, block[0])
        else:
            return conj(*(self.statement_requirements_to_z3(hyp,s) for s in block))
    
    def statement_requirements_to_z3(self,hyp:List[Z3Expr], s:Statement) -> Z3Expr:
        if isinstance(s,StateVarAssign):
            assert s.varop == ":="
            return conj(equals(z3primed(s.varname), self.term2z3def(s.value_expr)),
                        self.term_requirements_to_z3(hyp, s.value_expr))
        elif isinstance(s,IfElse):
            if s.false_branch:
                return conj(
                    implies(self.term2z3def(s.test),      self.block2z3def(s.true_branch)),
                    implies(neg(self.term2z3def(s.test)), self.block2z3def(s.false_branch))
                )
            else:
                return implies(self.term2z3def(s.test),   self.block2z3def(s.true_branch))
        elif isinstance(s, FVRequirement):
            return "true"
        raise NotImplementedError
        # if isinstance(s,)
    
    def term_requirements_to_z3(self,hyp:List[Z3Expr], t:Term) -> Z3Expr:
        pass
