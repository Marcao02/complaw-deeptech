from src.independent.util import chcaststr
from src.independent.util_for_str import mapjoin
from src.model.L4Contract import *

COLORS = ('Blue', 'Red')


def colorStr(role_id, role_ids):
    return COLORS[role_ids.index(role_id)]


def situationAsDotNodeStr(sit: Situation) -> str:
    # vulnerableActors = sit.vulnerableParties()
    # if len(vulnerableActors) == 1:
    #     return f"{sit.situation_id}[label={sit.situation_id},color={colorStr(vulnerableActors[0], all_actors)}]"
    # else:
    return f"{sit.situation_id}[label={sit.situation_id}]"

def actionAsDotNodeStr(act: Action) -> str:
    # vulnerableActors = sit.vulnerableParties()
    # if len(vulnerableActors) == 1:
    #     return f"{sit.situation_id}[label={sit.situation_id},color={colorStr(vulnerableActors[0], all_actors)}]"
    # else:
    return f'{act.action_id}[label={act.action_id},shape=box]'


def actionRuleAsDotArcStr(con: NextActionRule, l4file:L4Contract) -> str:
    srcid : str = con.src_id
    situation = l4file.situation(con.src_id)
    if situation.is_anon():
        srcid = chcaststr(situation.parent_action_id)

    if isinstance(con, PartyNextActionRule):
        return f"{srcid} -> {con.action_id}"
    elif isinstance(con, EnvNextActionRule):
        return f"{srcid} -> {con.action_id} [style=dashed]"
    else:
        raise NotImplementedError


def contractToDotFileStr(l4file: L4Contract) -> str:
    # graphname = l4file.construct_main_part.name[1]
    cleaned_graphname = "_".join(l4file.contract_name.split(' ')).replace('-','_')
    situations_to_depict = filter(lambda s: not s.is_anon(),# and not s.situation_id.startswith("Breached"),
                                       l4file.situations_iter())
    situation_nodes_str = mapjoin(lambda x: situationAsDotNodeStr(x),
                                situations_to_depict,
                                ";\n\t")
    # actions_to_depict = filter(lambda a: len(list(l4file.sources_of_action(a))) > 0, #not a.action_id.startswith("Breach"),
    #                                   l4file.actions_by_id.values())
    actions_to_depict = l4file.actions_by_id.values()
    # print(actions_to_depict)
    action_nodes_str = mapjoin(lambda x: actionAsDotNodeStr(x),
                               actions_to_depict,
                               ";\n\t")

    # actions_to_situations_str = mapjoin(
    #     lambda action: f"{action.action_id} -> {action.dest_situation_id} [style=dashed]", l4file.actions_iter(), ";\n\t")
    nonmultiloop_actions_to_situations_str = ""
    for action in l4file.actions_iter():
        if not action in actions_to_depict:
            continue
        # if is_derived_trigger_id(action.action_id):
        #     continue
        if not action.following_anon_situation:
            if action.dest_situation_id != LOOP_KEYWORD:
                nonmultiloop_actions_to_situations_str += f"{action.action_id} -> {action.dest_situation_id} [style=dashed];\n\t"
        else:
            pass

    multiloop_actions_to_situations_str = ""
    for situation in l4file.situations_iter():
        # if is_derived_destination_id(situation.situation_id):
        #     continue
        for action_rule in situation.action_rules():
            action = l4file.action(action_rule.action_id)
            if action.dest_situation_id == LOOP_KEYWORD:
                multiloop_actions_to_situations_str += f"{action.action_id} -> {situation.situation_id} [style=dashed];\n\t"

    action_rulesfrom_situations_str = mapjoin(lambda c: actionRuleAsDotArcStr(c, l4file), l4file.nextaction_rules(), ";\n\t")



    return f"""// THIS IS A GENERATED FILE. DO NOT EDIT.

digraph {cleaned_graphname} {{    
    Fulfilled[label=Fufilled];
    {situation_nodes_str}
    
    {'EnterFulfilled[label=EnterFufilled,shape=box];' if 'EnterFulfilled' in l4file.actions_by_id else ''} 
    {action_nodes_str}    
    
    {'EnterFulfilled -> Fulfilled;' if 'EnterFulfilled' in l4file.actions_by_id else ''}
    
    {action_rulesfrom_situations_str}
    
    {nonmultiloop_actions_to_situations_str}     
    
    {multiloop_actions_to_situations_str}   
}}"""

def contractToDotFile(l4file: L4Contract, rootpath, use_filename = True, verbose = False) -> None:
    if use_filename:
        if verbose:
            print("filename:", l4file.filename)
        cleaned_contract_name = l4file.filename[:-3]
    else:
        # replace spaces with underscores
        cleaned_contract_name = "_".join(l4file.contract_name.split(' '))

    if l4file.img_file_name:
        img_path = f"{rootpath}/{l4file.img_file_name}"
    else:
        img_path = f"{rootpath}/{cleaned_contract_name}.png"

    if l4file.dot_file_name:
        dot_path = f"{rootpath}/{l4file.dot_file_name}"
    else:
        dot_path = f"{rootpath}/{cleaned_contract_name}.dot"

    dot_contents = contractToDotFileStr(l4file)
    f = open(dot_path, 'w')
    f.write(dot_contents)
    f.close()
    command = f'dot -Tpng -o {img_path} "{dot_path}"'
    if verbose:
        print('\nTrying to run graphviz with:\n\t' + command)
    import os
    os.system(command)
    if verbose:
        print('If successful, generated png image will be at:\n\t' + img_path + '\n')