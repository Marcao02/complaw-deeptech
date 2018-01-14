from typing import List, Union, cast

from src.model.ActionRule import PartyNextActionRule
from src.model.BoundVar import GlobalVar, RuleBoundActionParam
from src.model.GlobalStateTransform import GlobalStateTransform
from src.model.GlobalStateTransformStatement import IfElse, GlobalVarAssignStatement, GlobalStateTransformStatement
from src.model.GlobalVarDec import GlobalVarDec
from src.model.Term import FnApp, Term
from src.model.constants_and_defined_types import DeonticKeyword, RoleId, ActionId, GlobalVarId, RuleBoundActionParamId, \
    ENV_ROLE
from src.model.util import todo_once, castid
from src.model.L4Contract import L4Contract, NextActionRule

PRACTICALLY_FOREVER = "999y"

def tdmapname(roleid:RoleId, actionid:ActionId, keyword:DeonticKeyword) -> GlobalVarId:
    if keyword == "must-later":
        return castid(GlobalVarId, f"{roleid}_must_{actionid}_by")
    else:
        assert keyword == "may-later"
        return castid(GlobalVarId, f"{roleid}_may_{actionid}_by")

def add_tdmap_dec(prog: L4Contract, mapvar_name: GlobalVarId) -> None:
    print("Adding global var dec " + mapvar_name)
    prog.global_var_decs[mapvar_name] = GlobalVarDec(mapvar_name, "TDMap", None, [])


def floating_rules_transpile_away(prog:L4Contract) -> None:

    # ---------------------------------
    # Removing from action declarations
    # ---------------------------------
    # In this section, the new TDMap global state variables are also added
    for far in prog.futureaction_rules():
        assert far.fixed_args, "Should not be using a floating rule for an action without arguments. Use a boolean variable instead."

        parent_action = prog.action(far.src_action_id)
        parent_action.futures.remove(far)
        timedelta_term : Union[str, Term]

        todo_once("Need to remove used-up rule instances also, in state transform...")

        if isinstance(far.time_constraint, FnApp):
            assert (far.time_constraint.head == "≤" or far.time_constraint.head == "<=")
            assert isinstance(far.time_constraint.args[0], FnApp) and far.time_constraint.args[0].head == "event_td"

            timedelta_term = far.time_constraint.args[1]
        else:
            todo_once("TEMP HACK FOR TYPECHECKING.")
            timedelta_term = cast(Term,PRACTICALLY_FOREVER)

        map_name = tdmapname(far.role_id, far.action_id, far.deontic_keyword)
        if map_name not in prog.global_var_decs:
            add_tdmap_dec(prog, map_name)
        map_dec = prog.global_var_decs[map_name]
        map_var = GlobalVar( map_dec )
        statement : GlobalStateTransformStatement
        if far.entrance_enabled_guard:
            statement = IfElse(far.entrance_enabled_guard,
                          [GlobalVarAssignStatement(
                              map_name,
                              FnApp("mapSet",[map_var,
                                              FnApp('tuple', far.fixed_args),
                                              timedelta_term])
                          )]
                        )
        else:
            statement = GlobalVarAssignStatement(
                               map_name,
                               FnApp("mapSet", [map_var,
                                                FnApp('tuple', far.fixed_args),
                                                timedelta_term])
                           )

        if not parent_action.global_state_transform:
            parent_action.global_state_transform = GlobalStateTransform([])
        parent_action.global_state_transform.statements.append(statement)

    # ----------------------------------------------
    # Removing from state (aka section) declarations
    # ----------------------------------------------
    for sec in prog.sections_iter():
        if len(sec.possible_floating_rule_types) == 0:
            continue

        for frt in sec.possible_floating_rule_types:
            rid = frt[0]
            aid = frt[1]
            kw = frt[2]
            action = prog.action(aid)

            map_name = tdmapname(rid, aid, kw)
            assert map_name in prog.global_var_decs, f"global state var {map_name} should've been added already. This is it: " + str(prog.global_var_decs)
            map_dec = prog.global_var_decs[map_name]
            map_var = GlobalVar(map_dec)

            assert rid != ENV_ROLE


            rule : PartyNextActionRule
            if kw == 'may-later':
                rule = PartyNextActionRule(sec.section_id, rid, aid, [], None, 'may')
                params = [RuleBoundActionParam(castid(RuleBoundActionParamId, "?" + str(i)), rule, i) for i in range(len(action.params))]
                rule.time_constraint = FnApp("tdGEQ",
                                             [map_var,
                                              FnApp('tuple', params),
                                              FnApp('event_td',[])
                                             ])
                rule.where_clause = FnApp('mapHas', [map_var, FnApp('tuple', params)])
                rule.args = list(map(lambda p: p.name, cast(List[RuleBoundActionParam], params)))
                sec.add_action_rule(rule)
            else:
                assert kw == 'must-later'
                rule = PartyNextActionRule(sec.section_id, rid, aid, [], None, 'may')
                params = [RuleBoundActionParam(castid(RuleBoundActionParamId, "?" + str(i)), rule, i) for i in
                          range(len(action.params))]
                rule.time_constraint = FnApp("tdGEQ",
                                             [map_var,
                                              FnApp('tuple', params),
                                              FnApp('event_td', [])
                                              ])
                rule.where_clause = FnApp('mapHas', [map_var, FnApp('tuple', params)])
                rule.args = list(map(lambda p: p.name, cast(List[RuleBoundActionParam], params)))
                sec.add_action_rule(rule)

                rule = PartyNextActionRule(sec.section_id, rid, aid, [], None, 'obligation-options-include')
                params = [RuleBoundActionParam(castid(RuleBoundActionParamId, "?" + str(i)), rule, i) for i in
                          range(len(action.params))]
                rule.time_constraint = FnApp("tdLT",
                                             [map_var,
                                              FnApp('tuple', params),
                                              FnApp('event_td', [])
                                              ])
                rule.where_clause = FnApp('mapHas', [map_var, FnApp('tuple', params)])
                rule.args = list(map(lambda p: p.name, cast(List[RuleBoundActionParam], params)))
                sec.add_action_rule(rule)

