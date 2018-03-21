from src.independent.util import castid
from src.model.ActionRule import NextActionRule, PartyNextActionRule, EnvNextActionRule
from src.model.Situation import Situation
from src.hard_correctness_checks.SMTLIB import *

from src.model.StateVarDec import StateVarDec
from src.model.Literal import Literal, SortLit, SimpleTimeDeltaLit, BoolLit
from src.model.Action import Action
from src.model.BoundVar import BoundVar, ActionBoundActionParam
from src.model.L4Contract import L4Contract
from src.model.Term import Term, FnApp
from src.parse_to_model.sexpr_to_L4Contract import isprimed, unprimed
from src.model.StateTransform import StateTransform
from src.model.Statement import LocalVarDec, StateVarAssign, IfElse, FVRequirement, Statement, Block


# def appendSMTLine(self, s: SMTLine, tolines:List[SMTLine]):
#     tolines.append(s)

# def push(self, tolst: List[SMTCommand_]):
#     tolst.append("(push)")
#     self.state_vars_updated_stack.append([])

# def initial_vals_imply_invariants(prog:L4Contract, lst:List[SMTLine]):
#
#     for (vname, vdec) in prog.state_var_decs.items():
#         if vdec.initval:
#             lst.append(equals(vname, ))

# class GeneratedSMTLibData:


def statevar_initialisations_ok(prog:L4Contract) -> List[SMTCommand_]:
    """This is basically ToSMTLib.term2smtlibdef"""
    pass


class ToSMTLIB:
    def __init__(self, prog:L4Contract, verbose:bool) -> None:
        self.verbose = verbose
        self.prog = prog

        self.cast_const_ind = 0

        # didn't use?
        # self.cast_const_decs : Dict[str, SMTCommand_] = dict()
        # self.cast_const_defs: Dict[str, SMTCommand_] = dict()
        self.cast_const_to_enclosing_action : Dict[str,str] = dict()

        self.cast_const_negated_type_asserts: Dict[str, SMTCommand_] = dict()

        self.stateTransformDefs: Dict[ActionId, SMTCommand_] = dict()

        self.curaid: ActionId = cast(ActionId,"")
        # self.stateTransforms: Dict[ActionId, List[Tuple[Z3Line,int]]] = dict()
        self.stateTransforms: Dict[ActionId, List[SMTLine]] = dict()

        self.commands_top: List[SMTLine] = []
        self.commands_for_actions: Dict[ActionId, List[SMTLine]] = dict()
        self.curindent: int = 0

        self.state_vars_updated_stack: List[List[str]] = []
        self.state_vars_updated: Set[str] = set()

        self.contractParamDecs : Dict[str, SMTCommand_] = dict()
        self.contractParamExtraTypeAssertions : Dict[str, SMTCommand_] = dict()
        self.contractParamDefns: Dict[str, SMTCommand_] = dict()

        # WARNING THIS CONTAINS THE PRIMED VERSIONS TOO
        self.stateVarDecs : Dict[str, SMTCommand_] = dict()
        self.stateVarExtraTypeAssertions: Dict[str, SMTCommand_] = dict()

        self.invariant_expressions: List[SMTExprNonatom_] = []
        self.invariant_true_assertions : List[SMTCommand_] = []
        self.invariant_maintained_conjectures: List[SMTExprNonatom_] = []
        # self.claims : List[Z3Statement] = []

        self.actionParamDecs : Dict[ActionId, Dict[str, SMTCommand_]] = dict()
        self.actionParamExtraTypeAssertions : Dict[ActionId, Dict[str, SMTCommand_]] = dict()


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
        self.append("(push)")
        self.state_vars_updated_stack.append([])

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
        # self.append(f'(echo "Should be sat:") (check-sat)')
        if msg:
            self.append(f'\t(echo "{msg}")')
        else:
            self.append(f'\t(echo "Try to prove {smtlib_expr_to_str(expr)}")')
        self.appendAssert(neg(expr))
        self.append("(check-sat)")
        self.pop()

    def invariantPrimed(self, inv:SMTExpr) -> SMTExpr:
        if isinstance(inv,str):
            if inv in self.stateVarDecs:
                if is_smt_primed_token(inv):
                    raise Exception("Can't use primed state variables in an Invariant.")
                else:
                    return to_smt_primed_token(inv)
            else:
                return inv
        elif isinstance(inv,(float,bool,int)):
            return inv
        else:
            assert isinstance(inv,SMTExprNonatom_), inv
            return SMTExprNonatom(inv.symb, tuple(self.invariantPrimed(x) for x in inv.args))

    def statevarDecs2smtlib(self):
        for svd in self.prog.state_var_decs.values():
            self.statevarDec2smtlib(svd)

    def statevarDec2smtlib(self, svd:StateVarDec):
        self.stateVarDecs[svd.name] = declareconst(svd.name, sort2smtlibprimtype(svd.sort))
        self.stateVarDecs[to_smt_primed_token(svd.name)] = declareconst(to_smt_primed_token(svd.name), sort2smtlibprimtype(svd.sort))
        if svd.sort in SORT_TO_PRED and isinstance(svd.sort, str):
            self.stateVarExtraTypeAssertions[svd.name] = assertexpr(SORT_TO_PRED[svd.sort](svd.name))
        elif self.verbose and svd.sort != 'Bool':
            print(f"Skipping extra type prop for state var {svd.name}:{svd.sort}")

    def contractParamDecs2smtlib(self):
        for cpd in self.prog.contract_params.values():
            self.contractParamDecs[cpd.name] = declareconst(cpd.name, sort2smtlibprimtype(cpd.sort))
            if cpd.sort in SORT_TO_PRED and isinstance(cpd.sort, str):
                self.contractParamExtraTypeAssertions[cpd.name] = assertexpr(SORT_TO_PRED[cpd.sort](cpd.name))
            elif self.verbose and cpd.sort != 'Bool':
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
            elif self.verbose and apsort != 'Bool':
                print(f"Skipping extra type prop for action param {scoped_apname}:{apsort}")

    def boilerplate_smtlib(self) -> Tuple[str,...]:
        rv = (
            "(declare-const event_td Int)",
            "(assert (> event_td 0))"
            "(declare-const next_event_td Int)",
            "(assert (>= next_event_td event_td))",

            # "(declare-sort TDMapNatNat)",
            # "(declare-fun mapSet (TDMapNatNat NatNat Real) TDMapNatNat)"
        )

        # 'mapSet': tdmapSet,
        # 'mapDelete': tdmapDelete,
        # 'mapHas': tdmapHas,

        # future_event_td should not be in the program after floating_rules_transpile_away
        # "(declare-const future_event_td Int)",
        # "(assert (>= future_event_td next_event_td))"
        return rv


    def prog2smtlibdef(self):
        self.contractParamDecs2smtlib()
        self.statevarDecs2smtlib()
        self.invariant2smtlib()

        for a in self.prog.actions_iter():
            self.action2smtlib(a)

        for s in self.prog.situations_iter():
            self.situation2smtlib(s)

    def invariant2smtlib(self):
        for invariant in self.prog.state_invariants:
            inv = self.term2smtlibdef(invariant.prop)
            self.invariant_expressions.append(inv)
            self.invariant_true_assertions.append(assertexpr(inv))
            self.invariant_maintained_conjectures.append(
                implies(
                    inv,
                    self.invariantPrimed(inv)
                )
            )
    def queueInvariantProofObligs(self):
        for invariant in self.prog.state_invariants:
            inv = self.term2smtlibdef(invariant.prop)
            self.appendProofOblig(inv, f"Invariant {str(inv)} holds in start state.")

    def action2smtlib(self, a:Action):
        self.curaid = a.action_id
        self.commands_for_actions[a.action_id] = []
        self.push()
        self.actionParams2smtlib(a)

        # preconditions hold upon entering
        if len(a.preconditions) > 0:
            self.append("; PRE begin")
            for pre in a.preconditions:
                self.appendAssert(self.term2smtlibdef(pre))
            self.append("; PRE end")

        # assert that invariants hold when entering the state transform:
        for inv_assert in self.invariant_true_assertions:
            self.append(inv_assert)

        self.stateTransform2smtlib(a.state_transform)

        for invcheck in self.invariant_maintained_conjectures:
            # invcheck has the form ('=>', e1, e2)
            hypoth_for_printing = invcheck.args[1]
            self.appendProofOblig(invcheck, f"{self.curaid} INV CHECK:" + smtlib_expr_to_str(hypoth_for_printing))
        self.pop()
        self.curaid = castid(ActionId, "")

    def situation2smtlib(self, sit:Situation):
        # print(sit.situation_id)
        strong_oblig_rules : List[PartyNextActionRule] = cast(List[PartyNextActionRule], list(filter(
            lambda x: isinstance(x,PartyNextActionRule) and x.deontic_keyword == 'must',
                                 sit.action_rules())))

        if len(strong_oblig_rules) > 1:
            enabled_conditions = list(map(lambda x: x.entrance_enabled_guard, strong_oblig_rules))
            assert all( map(lambda c: c is not None, enabled_conditions) )
            terms : List[SMTExpr] = list(map(lambda t: self.term2smtlibdef(cast(Term,t)), enabled_conditions))
            result = disjoint(*terms)
            # print(result)

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
        if len(are_same) > 0:
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

            elif t.fnsymb_name in ENV_VAR_SUBST:
                return t.fnsymb_name

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
            if isinstance(t, SimpleTimeDeltaLit):
                return t.num
            elif isinstance(t, BoolLit):
                return str(t.lit).lower()
            else:
                assert isinstance(t.lit, (str,int,float)), t
                return cast(SMTExpr, t.lit)
        else:
            raise NotImplementedError(f"self.term2smtlibdef({str(t)}), arg type {type(t)}, {isinstance(t, Literal)}")
