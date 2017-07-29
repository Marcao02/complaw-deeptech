# from typing import Union, List, Set, Dict, Any, Tuple, Callable, Iterator
from LSMTop import *

COLORS = ('Blue', 'Red')


def colorStr(actor_id, actor_ids):
    return COLORS[actor_ids.index(actor_id)]


def eventStateAsDotNodeStr(es: EventState, all_actors: List[str]) -> str:
    vulnerableActors = es.vulnerableParties()
    if len(vulnerableActors) == 1:
        return f"{es.name}[label={es.name},color={colorStr(vulnerableActors[0], all_actors)}]"
    else:
        return f"{es.name}[label={es.name}]"


def transitionAsDotArcStr(tc: TransitionClause) -> str:
    if tc.actor_id == NONACTION_BLOCK_LABEL:
        return f"{tc.src_id} -> {tc.dest_id} [style=dashed]"
    else:
        return f"{tc.src_id} -> {tc.dest_id}"


def contractToDotFileStr(l4file: L4Top) -> str:
    # the [1] is because first element is STRING_LITERAL tag
    # graphname = l4file.formal_contract.name[1]
    cleaned_graphname = "_".join(l4file.formal_contract.name[1].split(' '))
    nodes_str = mapjoin(lambda x: eventStateAsDotNodeStr(x, l4file.actors), l4file.event_states(), ";\n\t")
    rv = "digraph " + cleaned_graphname + " {\n\t"
    transitions_str = mapjoin(transitionAsDotArcStr, l4file.transitions(), ";\n\t")
    rv += nodes_str + ";\n\t" + transitions_str + ";\n}"
    return rv

def contractToDotFile(l4file: L4Top) -> None:
    outname = "out/state_diagram.png"
    cleaned_graphname = "_".join(l4file.formal_contract.name[1].split(' '))
    filestr = contractToDotFileStr(l4file)
    f = open(f"out/{cleaned_graphname}.dot", 'w')
    f.write(filestr)
    f.close()
    command = f'dot -Tpng -o{outname} "out/{cleaned_graphname}.dot"'
    print('\nTrying to run graphviz with:\n\t' + command)
    import os
    os.system(command)
    print('If successful result will be at ' + outname + '\n')