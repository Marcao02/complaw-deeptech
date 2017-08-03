from typing import Any, List, Iterable, Callable, Dict

Sort = str
EventStateId = str
ActorId = str
ProseClauseId = str
GlobalVarId = str

ProseContract = Dict[str,str]
ParamsDec = Dict[str,Sort]

SPECIAL_CONSTANTS = {'MAX_TIME', 'MAX_EVENT_STATE_CHANGES'}
VARIABLE_MODIFIERS = {'writeonce', 'writeonly'}

DEADLINE_OPERATORS = {'by', 'within', 'on' ,'at', 'before'}
DEADLINE_KEYWORDS = {'immediately', 'nodeadline'}
DURATION_SYMBOLS = {'discretionary'}

# These are not actually case sensitive
GLOBAL_VARS_SECTION_LABEL = "GlobalVars"
CLAIMS_SECTION_LABEL = "Claims"
PROSE_CONTRACT_SECTION_LABEL = "ProseContract"
FORMAL_CONTRACT_SECTION_LABEL = "FormalContract"
ACTORS_SECTION_LABEL = "Actors"

CONTRACT_PARAMETERS_SECTION_LABEL = "Parameters"
DOT_FILE_NAME_LABEL = "DotFileName"
IMG_FILE_NAME_LABEL = "ImgFileName"
EVENT_STATES_SECTION_LABEL = "EventStates"

CODE_BLOCK_LABEL = "Entrance"
NONACTION_BLOCK_LABEL = "Fallbacks"

def streqci(s1:Any,s2:Any) -> bool:
    # ci for case insensitive
    return isinstance(s1,str) and isinstance(s2,str) and s1.lower() == s2.lower()

def is_singleton_string_list(lst:List[Any]) -> bool:
    return len(lst) == 1 and isinstance(lst[0],str)

def list_split(by:str, lst:List[str]) -> List[List[str]]:
    lists = []
    next : List[str] = []
    for x in lst:
        if x == by:
            lists.append(next)
            next = []
        else:
            next.append(x)
    if len(next) > 0:
        lists.append(next)
    return lists

def mapjoin(f:Callable[[Any],str], iter:Iterable[Any], delim:str) -> str:
    return delim.join(map(f,iter))
