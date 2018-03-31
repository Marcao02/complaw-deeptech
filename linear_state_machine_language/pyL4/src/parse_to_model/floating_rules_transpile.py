from typing import List, Union, cast

from src.model.Sort import SortOpApp, Sort
from src.independent.util import todo_once, castid
from src.constants_and_defined_types import DeonticKeyword, RoleId, ActionId, StateVarId, RuleBoundActionParamId, \
    ENV_ROLE, ActionBoundActionParamId
from src.model.ActionRule import PartyNextActionRule
from src.model.BoundVar import RuleBoundActionParam, ActionBoundActionParam
from src.model.StateTransform import StateTransform
from src.model.Statement import IfElse, StateVarAssign, Statement
from src.model.StateVarDec import StateVarDec
from src.model.L4Contract import L4Contract
from src.model.Literal import SimpleTimeDeltaLit, RoleIdLit
from src.model.Term import FnApp, Term

PRACTICALLY_FOREVER = SimpleTimeDeltaLit(999*52,"w")

def tdmapname(roleid:RoleId, actionid:ActionId, keyword:DeonticKeyword) -> StateVarId:
    if keyword == "must-later":
        return castid(StateVarId, f"{roleid}_must_{actionid}_by")
    else:
        assert keyword == "may-later"
        return castid(StateVarId, f"{roleid}_may_{actionid}_by")

def floating_rules_transpile_away(prog:L4Contract, verbose:bool) -> None:
    def add_tdmap_dec(prog: L4Contract, mapvar_name: StateVarId, argsort: Sort) -> None:
        if verbose:
            print("Adding state var dec " + mapvar_name)
        prog.state_var_decs[mapvar_name] = StateVarDec(mapvar_name, SortOpApp.c("TDMap", (argsort,)),
                                                        FnApp('emptyTDMap', []), [])
    statement: Statement
    params: List[Term]
    ifelse: IfElse

    def pack(_params:List[Term]):
        return FnApp('tuple',_params) if len(_params) > 1 else _params[0]

    # --------------------------------------------------------------
    # Deleting instances of rules when a corresponding action occurs
    # --------------------------------------------------------------
    for action in prog.actions_iter():
        for fut_rule_type in prog.possible_floating_rule_types:
            if fut_rule_type.aid != action.action_id:
                continue

            map_name = tdmapname(fut_rule_type.rid, fut_rule_type.aid, fut_rule_type.kw)
            if map_name not in prog.state_var_decs:
                if len(action.param_sorts_by_name) == 1:
                    add_tdmap_dec(prog, map_name, action.param_sort(0))
                elif len(action.param_sorts_by_name) == 2:
                    add_tdmap_dec(prog, map_name,
                                  SortOpApp.c('Tuple', tuple(action.param_sort(i) for i in range(len(action.param_sorts_by_name))))
                                  )
            map_dec = prog.state_var_decs[map_name]
            map_var = prog.new_state_var_ref(map_name)

            # now the statetransform will need to check that both the role and
            # the action params matchTerm. this requires a role environment variable.
            params = [ActionBoundActionParam(castid(ActionBoundActionParamId, action.param_names[i]), action, i) for i in
                      range(len(action.param_names))]
            ifelse = IfElse(FnApp("==", [FnApp("event_role",[]), RoleIdLit(fut_rule_type.rid)]),
                               [StateVarAssign(
                                   map_dec,
                                   FnApp("mapDelete", [map_var,
                                                       pack(params) ])
                               )]
                               )
            ifelse.true_branch[0].grandparent_ifelse = ifelse

            if not action.state_transform:
                action.state_transform = StateTransform([])
            action.state_transform.statements.append(ifelse)
            ifelse.parent_block = action.state_transform.statements

    # ---------------------------------
    # Removing from action declarations
    # ---------------------------------
    # In this section, the new TDMap state variables are also added
    for far in prog.futureaction_rules():
        assert far.fixed_args, "Should not be using a floating rule for an action without arguments. Use a boolean variable instead."

        parent_action = prog.action(far.src_action_id)
        parent_action.futures.remove(far)
        timedelta_term : Union[str, Term]

        if isinstance(far.time_constraint, FnApp):
            assert (far.time_constraint.head == "≤" or far.time_constraint.head == "<=")
            assert isinstance(far.time_constraint.args[0], FnApp) and far.time_constraint.args[0].head == "future_event_td"

            timedelta_term = far.time_constraint.args[1]
        else:
            todo_once("TEMP HACK FOR TYPECHECKING.")
            timedelta_term = PRACTICALLY_FOREVER

        map_name = tdmapname(far.role_id, far.action_id, far.deontic_keyword)
        if map_name not in prog.state_var_decs:
            if len(action.param_sorts_by_name) == 1:
                add_tdmap_dec(prog, map_name, action.param_sort(0))
            elif len(action.param_sorts_by_name) == 2:
                add_tdmap_dec(prog, map_name,
                              SortOpApp.c('Tuple',
                                        tuple(action.param_sort(i) for i in range(len(action.param_sorts_by_name))))
                              )
        map_dec = prog.state_var_decs[map_name]
        map_var = prog.new_state_var_ref(map_name)
        statement2 : Statement
        if far.entrance_enabled_guard:
            ifelse = IfElse(far.entrance_enabled_guard,
                          [StateVarAssign(
                              map_dec,
                              FnApp("mapSet",[map_var,
                                              pack(far.fixed_args),
                                              timedelta_term])
                          )]
                        )
            ifelse.true_branch[0].grandparent_ifelse = ifelse
            statement = statement2
        else:
            statement2 = StateVarAssign(
                               map_dec,
                               FnApp("mapSet", [map_var,
                                                pack(far.fixed_args),
                                                timedelta_term])
                           )

        if not parent_action.state_transform:
            parent_action.state_transform = StateTransform([])
        parent_action.state_transform.statements.append(statement2)
        statement2.parent_block = parent_action.state_transform.statements

    # ----------------------------------------------
    # Removing from situation declarations
    # ----------------------------------------------
    for sit in prog.situations_iter():
        if len(sit.possible_floating_rule_types) == 0:
            continue

        for frt in sit.possible_floating_rule_types:
            rid = frt[0]
            aid = frt[1]
            kw = frt[2]
            action = prog.action(aid)

            map_name = tdmapname(rid, aid, kw)
            assert map_name in prog.state_var_decs, f"state var {map_name} should've been added already. This is it: " + str(prog.state_var_decs)
            map_var = prog.new_state_var_ref(map_name)

            assert rid != ENV_ROLE

            map_nonempty_term = FnApp('nonempty', [map_var])
            # params : List[Term]
            rule : PartyNextActionRule
            if kw == 'may-later':
                rule = PartyNextActionRule(sit.situation_id, rid, aid, [], map_nonempty_term, castid(DeonticKeyword,'may'))
                params = [RuleBoundActionParam(castid(RuleBoundActionParamId, "?" + str(i)), rule, i) for i in range(len(action.param_names))]
                rule.time_constraint =  FnApp("tdGEQ",
                                             [map_var,
                                              pack(params),
                                              FnApp('next_event_td',[])
                                             ])
                rule.where_clause = FnApp('mapHas', [map_var, pack(params)])
                rule.arg_vars_bound_by_rule = list(map(lambda p: p.name, cast(List[RuleBoundActionParam], params)))
                sit.add_action_rule(rule)
            else:
                assert kw == 'must-later'
                # rule = PartyNextActionRule(sit.situation_id, rid, aid, [], None, 'may')
                # params = [RuleBoundActionParam(castid(RuleBoundActionParamId, "?" + str(i)), rule, i) for i in
                #           range(len(action.params))]
                # rule.time_constraint = FnApp("tdGEQ",
                #                              [map_var,
                #                               pack(params),
                #                               FnApp('last_event_td', [])
                #                               ])
                # rule.where_clause = FnApp('mapHas', [map_var, pack(params)])
                # rule.args = list(map(lambda p: p.name, cast(List[RuleBoundActionParam], params)))
                # sit.add_action_rule(rule)

                rule = PartyNextActionRule(sit.situation_id, rid, aid, [], map_nonempty_term, castid(DeonticKeyword,'quasi-responsibility'))
                params = [RuleBoundActionParam(castid(RuleBoundActionParamId, "?" + str(i)), rule, i) for i in
                          range(len(action.param_names))]
                rule.time_constraint = FnApp("tdGEQ",
                                             [map_var,
                                              pack(params),
                                              FnApp('next_event_td', [])
                                              ])
                rule.where_clause = FnApp('mapHas', [map_var, pack(params)])
                rule.arg_vars_bound_by_rule = list(map(lambda p: p.name, cast(List[RuleBoundActionParam], params)))
                sit.add_action_rule(rule)

