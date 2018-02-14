from typing import NewType

from src.model.StateTransform import StateTransform
from src.independent.util import mapjoin
from src.parse_to_model.sexpr_to_L4Contract import isprimed, unprimed
from src.constants_and_defined_types import ActionId
from src.model.Sort import Sort
from src.model.StateVarDec import StateVarDec
from src.model.Literal import Literal, SortLit
from src.model.Action import Action
from src.model.BoundVar import BoundVar, ActionBoundActionParam
from src.model.L4Contract import L4Contract
from src.model.Term import Term, FnApp
from src.independent.typing_imports import *
from src.model.Statement import LocalVarDec, StateVarAssign, IfElse, FVRequirement, Statement, Block


SMTNonatomicExpr = NewType('SMTNonatomicExpr', Tuple[Any])
SMTAtom = Union[str, int, float, bool]
SMTExpr = Union[SMTNonatomicExpr, SMTAtom]
SMTCommand = NewType('SMTCommand', Tuple[Any])
SMTLine = Union[SMTCommand, str]

SMT_BUILDIN_FNS = frozenset({'and', 'or', 'not', '=>', '=', '*', '+', '>', '<', '<=', '>=', '/', '-'})

SORT_TO_PRED : Dict[str,Callable[[str], SMTExpr]]= {
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

def disj(*args:SMTExpr) -> SMTNonatomicExpr:
    return cast(SMTNonatomicExpr, ("or", *args))
def conj(*args:SMTExpr) -> SMTNonatomicExpr:
    return cast(SMTNonatomicExpr, ("and", *args))
def neg(arg:SMTExpr) -> SMTNonatomicExpr:
    return cast(SMTNonatomicExpr, ("not", arg))
def implies(arg1:SMTExpr, arg2:SMTExpr) -> SMTNonatomicExpr:
    return cast(SMTNonatomicExpr, ("=>", arg1, arg2))
    # return cast(Z3NonatomicExpr, ("or",("not",arg1), arg2))
def equals(arg1:SMTExpr, arg2:SMTExpr) -> SMTNonatomicExpr:
    return cast(SMTNonatomicExpr, ("=", arg1, arg2))
def ite(a1:SMTExpr, a2:SMTExpr, a3:SMTExpr) -> SMTNonatomicExpr:
    return cast(SMTNonatomicExpr, ('ite', a1, a2, a3))

def fnapp(symb:str, *args:SMTExpr) -> SMTNonatomicExpr:
    return cast(SMTNonatomicExpr, (symb, *args))

def declareconst(const:str, sort:str) -> SMTCommand:
    return cast(SMTCommand, ('declare-const', const, sort))
def assertexpr(expr:SMTExpr) -> SMTCommand:
    return cast(SMTCommand, ('assert' , expr))


def to_smt_primed_token(s:str) -> str:
    return s + "_next"
def to_smt_unprimed_token(s:str) -> str:
    assert s.endswith("_next")
    return s[-5:]
def is_smt_primed_token(s:str):
    return s.endswith("_next")

def rename_with_action_scope(actionparam_name:str, actionid:str) -> str:
    return actionparam_name + "_" + actionid



def smtlib_expr_to_str(x:Union[SMTExpr, str]) -> str:
    if isinstance(x,(str,int,float,bool)):
        return str(x)
    else:
        return f"({mapjoin(smtlib_expr_to_str, x, ' ')})"

INDENT_SIZE = 2
def smt_lines_to_str(lines:List[SMTLine]) -> str:
    rv = ""
    indent = 0
    for line in lines:
        if line == "(pop)":
            indent -= 1

        if isinstance(line,str):
            rv += f"{indent*INDENT_SIZE*' '}{line}\n"
        else:
            rv += f"{indent*INDENT_SIZE*' '}({mapjoin(smtlib_expr_to_str, line, ' ')})\n"

        if line == "(push)":
            indent += 1
    return rv


def sort2smtlibprimtype(s: Sort) -> str:
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

class ToSMTLIB:
    def __init__(self, prog:L4Contract) -> None:
        self.prog = prog

        self.cast_const_ind = 0

        self.cast_const_decs : Dict[str, SMTCommand] = dict()
        self.cast_const_defs: Dict[str, SMTCommand] = dict()
        self.cast_const_to_enclosing_action : Dict[str,str] = dict()

        self.cast_const_negated_type_asserts: Dict[str, SMTCommand] = dict()

        self.stateTransformDefs: Dict[ActionId, SMTCommand] = dict()

        self.curaid: ActionId = cast(ActionId,"")
        # self.stateTransforms: Dict[ActionId, List[Tuple[Z3Line,int]]] = dict()
        self.stateTransforms: Dict[ActionId, List[SMTLine]] = dict()

        self.commands_top: List[SMTLine] = []
        self.commands_for_actions: Dict[ActionId, List[SMTLine]] = dict()
        self.curindent: int = 0

        self.state_vars_updated_stack: List[List[str]] = []
        self.state_vars_updated: Set[str] = set()

        self.contractParamDecs : Dict[str, SMTCommand] = dict()
        self.contractParamExtraTypeAssertions : Dict[str, SMTCommand] = dict()
        self.contractParamDefns: Dict[str, SMTCommand] = dict()

        # WARNING THIS CONTAINS THE PRIMED VERSIONS TOO
        self.stateVarDecs : Dict[str, SMTCommand] = dict()
        self.stateVarExtraTypeAssertions: Dict[str, SMTCommand] = dict()

        self.invariant_assertions : List[SMTCommand] = []
        self.invariant_conjectures: List[SMTExpr] = []
        # self.claims : List[Z3Statement] = []

        self.actionParamDecs : Dict[ActionId, Dict[str, SMTCommand]] = dict()
        self.actionParamExtraTypeAssertions : Dict[ActionId, Dict[str, SMTCommand]] = dict()


    def noteStateVarUpdated(self,var:str):
        self.state_vars_updated_stack[-1].append(var)
        self.state_vars_updated.add(var)

    def append(self, s:SMTLine):
        if self.curaid == "":
            self.commands_top.append(s)
        else:
            # self.stateTransforms[self.curaid].append((s,self.curindent))
            self.commands_for_actions[self.curaid].append(s)
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
    def appendAssert(self, expr:SMTExpr):
        self.append(assertexpr(expr))
    def appendConstDec(self, name:str, sort:str):
        self.append(declareconst(name,sort))

    def appendProofOblig(self, expr:SMTExpr, msg:str = ""):
        self.push()
        if msg:
            self.append(f'(echo "{msg}")')
        else:
            self.append(f'(echo "Try to prove {smtlib_expr_to_str(expr)}")')
        self.appendAssert(neg(expr))
        self.append("(check-sat)")
        self.pop()

    def invariantPrimed(self, inv:SMTExpr):
        if isinstance(inv,str):
            if inv in self.stateVarDecs:
                if is_smt_primed_token(inv):
                    raise Exception("Can't use primed state variables in an Invariant.")
                else:
                    return to_smt_primed_token(inv)
            else:
                return inv
        else:
            return tuple(self.invariantPrimed(x) for x in cast(SMTNonatomicExpr, inv))

    def statevarDecs2smtlib(self):
        for svd in self.prog.global_var_decs.values():
            self.statevarDec2smtlib(svd)

    def statevarDec2smtlib(self, svd:StateVarDec):
        self.stateVarDecs[svd.name] = declareconst(svd.name, sort2smtlibprimtype(svd.sort))
        self.stateVarDecs[to_smt_primed_token(svd.name)] = declareconst(to_smt_primed_token(svd.name), sort2smtlibprimtype(svd.sort))
        if svd.sort in SORT_TO_PRED and isinstance(svd.sort, str):
            self.stateVarExtraTypeAssertions[svd.name] = assertexpr(SORT_TO_PRED[svd.sort](svd.name))
        else:
            print(f"Skipping extra type prop for state var {svd.name}:{svd.sort}")

    def contractParamDecs2smtlib(self):
        for cpd in self.prog.contract_params.values():
            self.contractParamDecs[cpd.name] = declareconst(cpd.name, sort2smtlibprimtype(cpd.sort))
            if cpd.sort in SORT_TO_PRED and isinstance(cpd.sort, str):
                self.contractParamExtraTypeAssertions[cpd.name] = assertexpr(SORT_TO_PRED[cpd.sort](cpd.name))
            else:
                print(f"Skipping extra type prop for contract param {cpd.name}:{cpd.sort}")
            if cpd.value_expr is not None:
                self.contractParamDefns[cpd.name] = assertexpr(equals(cpd.name, self.term2smtlibdef(cpd.value_expr)))

    def actionParams2smtlib(self, action:Action):
        aid = action.action_id
        self.actionParamDecs[aid] = dict()
        self.actionParamExtraTypeAssertions[aid] = dict()
        for apname,apsort in action.param_sorts_by_name.items():
            scoped_apname = rename_with_action_scope(apname, aid)

            self.actionParamDecs[aid][scoped_apname] = declareconst(scoped_apname, sort2smtlibprimtype(apsort))
            self.append(self.actionParamDecs[aid][scoped_apname])
            if apsort in SORT_TO_PRED:
                assert isinstance(apsort,str)
                self.actionParamExtraTypeAssertions[aid][scoped_apname] = assertexpr(SORT_TO_PRED[apsort](scoped_apname))
                self.append(self.actionParamExtraTypeAssertions[aid][scoped_apname])
            else:
                print(f"Skipping extra type prop for action param {scoped_apname}:{apsort}")

    def prog2smtlibdef(self):
        self.contractParamDecs2smtlib()
        self.statevarDecs2smtlib()
        self.invariant2smtlib()

        for a in self.prog.actions_iter():
            self.action2smtlib(a)

    def invariant2smtlib(self):
        for invariant in self.prog.state_invariants:
            inv = self.term2smtlibdef(invariant.prop)
            self.invariant_assertions.append(assertexpr(inv))
            self.invariant_conjectures.append(
                implies(
                    inv,
                    self.invariantPrimed(inv)
                )
            )

    def action2smtlib(self, a:Action):
        self.curaid = a.action_id
        self.commands_for_actions[a.action_id] = []
        self.push()
        self.actionParams2smtlib(a)
        if len(a.preconditions) > 0:
            self.append("; PRE begin")
            for pre in a.preconditions:
                self.appendAssert(self.term2smtlibdef(pre))
            self.append("; PRE end")
        self.stateTransform2smtlib(a.global_state_transform)
        for invcheck in self.invariant_conjectures:
            # invcheck has the form ('=>', e1, e2)
            self.appendProofOblig(invcheck, f"{self.curaid} INV CHECK:" + smtlib_expr_to_str(invcheck[1])) # type:ignore
        self.pop()

    def stateTransform2smtlib(self, st:Optional[StateTransform]):

        self.block2smtlib(st.statements if st else [])

    def block2smtlib(self,block:Block):
        # assert len(block) > 0
        for s in block:
            self.statement2smtlib(s)
        are_same : List[SMTExpr] = []
        for var in self.stateVarDecs:
            if var not in self.state_vars_updated and not is_smt_primed_token(var):
                are_same.append(equals(to_smt_primed_token(var), var))
        self.appendAssert(conj(*are_same))

    def statement2smtlib(self, s:Statement):
        if isinstance(s, StateVarAssign):
            assert s.varop == ":="
            self.appendAssert(equals(to_smt_primed_token(s.varname), self.term2smtlibdef(s.value_expr)))
            self.noteStateVarUpdated(s.varname)

        elif isinstance(s, IfElse):
            self.push()
            self.appendAssert(self.term2smtlibdef(s.test))
            self.block2smtlib(s.true_branch)
            self.pop()

            if s.false_branch is not None:
                self.push()
                self.appendAssert(neg(self.term2smtlibdef(s.test)))
                self.block2smtlib(s.false_branch)
                self.pop()

        elif isinstance(s, FVRequirement):
            expr = self.term2smtlibdef(s.value_expr)
            self.appendProofOblig(expr, f"STATIC ASSERT: {smtlib_expr_to_str(expr)}")

        else:
            raise NotImplementedError(str(s))

    def new_cast_const(self,actionid:str) -> str:
        self.cast_const_ind += 1
        rv = "castconst" + str(self.cast_const_ind)
        self.cast_const_to_enclosing_action[rv] = actionid
        return rv
    

    def term2smtlibdef(self,t:Term) -> SMTExpr:
        if isinstance(t, FnApp):
            if t.fnsymb_name in SMT_BUILDIN_FNS:
                return fnapp(t.fnsymb_name, *[self.term2smtlibdef(arg) for arg in t.args])

            elif t.fnsymb_name == "cast" or t.fnsymb_name == "check":
                assert isinstance(t.args[0], SortLit)
                cc = self.new_cast_const(self.curaid)
                sort = t.args[0].lit
                primtype = sort2smtlibprimtype(sort)
                value_expr = self.term2smtlibdef(t.args[1])
                self.push()
                self.appendConstDec(cc, primtype)
                self.appendAssert(equals(cc, value_expr))
                proofoblig = SORT_TO_PRED[sort](cc)
                self.appendProofOblig(proofoblig, f"CAST VERIFY: {smtlib_expr_to_str(proofoblig)}")
                self.pop()
                return value_expr

            elif t.fnsymb_name == "units" or t.fnsymb_name == "trust":
                return self.term2smtlibdef(t.args[1])

            elif t.fnsymb_name in FN_NAME_SUBST:
                return fnapp(FN_NAME_SUBST[t.fnsymb_name], *[self.term2smtlibdef(arg) for arg in t.args])

            else:
                assert t.fnsymb_name in MACRO_DEFINED_FNS, t.fnsymb_name + " unhandled"
                return cast(SMTExpr, MACRO_DEFINED_FNS[t.fnsymb_name](*(self.term2smtlibdef(arg) for arg in t.args)))

        elif isinstance(t, BoundVar):
            assert not isinstance(t, LocalVarDec)
            if isprimed(t.name):
                return to_smt_primed_token(unprimed(t.name))

            if isinstance(t, ActionBoundActionParam):
                return rename_with_action_scope(t.name, t.action.action_id)

            return t.name
        elif isinstance(t, Literal):
            assert isinstance(t.lit, (str,int,float,bool))
            return cast(SMTExpr, t.lit)
        else:
            raise NotImplementedError(f"self.term2smtlibdef({str(t)}), arg type {type(t)}, {isinstance(t, Literal)}")
