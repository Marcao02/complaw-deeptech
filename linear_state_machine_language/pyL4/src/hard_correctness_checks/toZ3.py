from src.constants_and_defined_types import ActionBoundActionParamId, ActionId
from src.model.Sort import Sort
from src.model.StateVarDec import StateVarDec
from src.model.Literal import Literal, SortLit
from src.model.Action import Action
from src.model.BoundVar import GlobalVar, BoundVar
from src.model.L4Contract import L4Contract
from src.model.Term import Term, FnApp
from src.independent.typing_imports import *
from src.model.Statement import LocalVarDec, StateVarAssign, IfElse, FVRequirement, Statement
Block = List[Statement]

# Z3Expr = Union[str, int, Tuple[Any, ...]]
# Z3Statement = Tuple[Union[str, Z3Expr], ...]
Z3Expr = Any
Z3Statement = Any

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

def disj(*args:Z3Expr) -> Z3Expr:
    return ("or",) + args # type:ignore
def conj(*args:Z3Expr) -> Z3Expr:
    return ("and",) + args # type:ignore
def neg(arg:Z3Expr) -> Z3Expr:
    return "not", arg # type:ignore
def implies(arg1:Z3Expr, arg2:Z3Expr) -> Z3Expr:
    return "or",("not",arg1), arg2 # type:ignore
def equals(arg1:Z3Expr, arg2:Z3Expr) -> Z3Expr:
    return "=", arg1, arg2
def ite(a1:Z3Expr, a2:Z3Expr, a3:Z3Expr) -> Z3Expr:
    return 'ite',a1,a2,a3
def fnapp(symb:str, *args:Z3Expr) -> Z3Expr:
    return (symb,) + args # type:ignore

def declareconst(const:str, sort:str) -> Z3Expr:
    return 'declare-const', const, sort

def assertexpr(expr:Z3Expr) -> Z3Statement:
    return 'assert' , expr

def primed(s:str) -> str:
    return s + "_next"

MACRO_DEFINED_FNS : Dict[str, Callable] = {
    'cast': lambda S,t: t,
    'ifthenelse': lambda a,b,c: ite(a,b,c),
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
    def __init__(self) -> None:
        self.cast_const_decs : Dict[str, Z3Statement] = dict()
        self.cast_const_defs: Dict[str, Z3Expr] = dict()
        self.cast_const_negated_type_asserts: Dict[str, Z3Expr] = dict()
        self.cast_const_ind = 0
        self.stateVarDecs : Dict[str,Z3Statement] = dict()
        self.stateVarDecsExtra: Dict[str,Z3Statement] = dict()
        self.actionParamDecs : Dict[ActionId, Dict[str,Z3Statement]] = dict()
        self.actionParamDecsExtra : Dict[ActionId, Dict[str, Z3Statement]] = dict()
        self.stateTransformDecsZ3 : Dict[ActionId,Z3Expr] = dict()

    def statevarDec2z3(self, svd:StateVarDec):
        self.stateVarDecs[svd.name] = declareconst(svd.name, sort2z3primtype(svd.sort)),
        if svd.sort in SORT_TO_PRED and isinstance(svd.sort, str):
            self.stateVarDecsExtra[svd.name] = assertexpr(SORT_TO_PRED[svd.sort](svd.name))
        else:
            print("Skipping " + str(svd.sort) )

    def actionParams2z3(self, action:Action):
        self.actionParamDecs[action.action_id] = dict()
        self.actionParamDecsExtra[action.action_id] = dict()
        for apname,apsort in action.param_sorts_by_name.items():
            self.actionParamDecs[action.action_id][apname] = declareconst(apname, sort2z3primtype(apsort))
            if apsort in SORT_TO_PRED:
                assert isinstance(apsort,str)
                self.actionParamDecsExtra[action.action_id][apname] = SORT_TO_PRED[apsort](apname)
            else:
                print(f"Skipping {apname}:{apsort}")

    def prog2z3def(self, prog:L4Contract):
        for svd in prog.global_var_decs.values():
            self.statevarDec2z3(svd)

        for a in prog.actions_iter():
            self.actionParams2z3(a)
            self.action2z3def(a)

    def action2z3def(self, a:Action):
        if a.global_state_transform:
            self.stateTransformDecsZ3[a.action_id] = self.block2z3def(a.global_state_transform.statements)
    
    def block2z3def(self,block:Block) -> Z3Expr:
        assert len(block) > 0
        if len(block) == 1:
            return self.statement2z3def(block[0])
        else:
            return conj(*(self.statement2z3def(s) for s in block))
    
    def statement2z3def(self,s:Statement) -> Z3Expr:
        if isinstance(s,StateVarAssign):
            assert s.varop == ":="
            return equals(primed(s.varname), self.term2z3def(s.value_expr))
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
    
    def new_cast_const(self,) -> str:
        self.cast_const_ind += 1
        return "castconst" + str(self.cast_const_ind)
    

    def term2z3def(self,t:Term) -> Z3Expr:
        if isinstance(t, FnApp):
            if t.fnsymb_name in Z3_BUILDIN_FNS:
                return fnapp(t.fnsymb_name, *[self.term2z3def(arg) for arg in t.args])
            elif t.fnsymb_name == "cast":
                cc = self.new_cast_const()
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
            else:
                assert t.fnsymb_name in MACRO_DEFINED_FNS, t.fnsymb_name + " unhandled"
                return MACRO_DEFINED_FNS[t.fnsymb_name]( *(self.term2z3def(arg) for arg in t.args) )
        elif isinstance(t, BoundVar):
            assert not isinstance(t, LocalVarDec)
            return t.name
        elif isinstance(t, Literal):
            return t.lit
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
            return conj( equals(primed(s.varname), self.term2z3def(s.value_expr)),
                         self.term_requirements_to_z3(hyp, s.value_expr) )
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
