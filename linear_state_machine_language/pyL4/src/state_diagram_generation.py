# from typing import Union, List, Set, Dict, Any, Tuple, Callable, Iterator
from model.util import mapjoin
from model.L4Contract import *

COLORS = ('Blue', 'Red')


def colorStr(role_id, role_ids):
    return COLORS[role_ids.index(role_id)]


def sectionAsDotNodeStr(sect: Section, all_actors: List[str]) -> str:
    # vulnerableActors = sect.vulnerableParties()
    # if len(vulnerableActors) == 1:
    #     return f"{sect.section_id}[label={sect.section_id},color={colorStr(vulnerableActors[0], all_actors)}]"
    # else:
    return f"{sect.section_id}[label={sect.section_id}]"

def actionAsDotNodeStr(act: Action, all_actors: List[str]) -> str:
    # vulnerableActors = sect.vulnerableParties()
    # if len(vulnerableActors) == 1:
    #     return f"{sect.section_id}[label={sect.section_id},color={colorStr(vulnerableActors[0], all_actors)}]"
    # else:
    return f'{act.action_id}[label={act.action_id},shape=box]'


def connectionAsDotArcStr(con: Connection) -> str:
    if con.role_id == ENV_ROLE:
        return f"{con.src_id} -> {con.action_id} [style=dashed]"
    else:
        return f"{con.src_id} -> {con.action_id}"


def contractToDotFileStr(l4file: L4Contract) -> str:
    # graphname = l4file.formal_contract.name[1]
    cleaned_graphname = "_".join(l4file.contract_name.split(' '))
    nodes_str = mapjoin(lambda x: sectionAsDotNodeStr(x, l4file.roles), l4file.sections_iter(), ";\n\t")
    actions_str = mapjoin(lambda x: actionAsDotNodeStr(x, l4file.roles), l4file.actions_by_id.values(), ";\n\t")
    rv = "// THIS IS A GENERATED FILE. DO NOT EDIT.\n\n"
    rv += "digraph " + cleaned_graphname + " {\n\t"
    transitions_str = mapjoin(connectionAsDotArcStr, l4file.connections, ";\n\t")
    rv += actions_str + ";\n\t" + nodes_str + ";\n\t" + transitions_str + ";\n}"
    return rv

def contractToDotFile(l4file: L4Contract, rootpath = 'out', verbose=False) -> None:
    # replace spaces with underscores
    print(l4file.contract_name)
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
    command = f'dot -Tpng -o{img_path} "{dot_path}"'
    if verbose:
        print('\nTrying to run graphviz with:\n\t' + command)
    import os
    os.system(command)
    if verbose:
        print('If successful generated png image will be at:\n\t' + img_path + '\n')