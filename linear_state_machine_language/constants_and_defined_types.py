from typing import Dict, Union, List

Sort = str
EventStateId = str
ActorId = str
ProseClauseId = str
GlobalVarId = str

SExpr = Union[str,List[any]]

ProseContract = Dict[str,str]
ParamsDec = Dict[str,Sort]

SPECIAL_CONSTANTS = {'MAX_TIME', 'MAX_EVENT_STATE_CHANGES'}
VARIABLE_MODIFIERS = {'writeonce', 'writeonly', 'writeAtMostOnce',
                      'writeOnceMore', 'inconly', 'deconly',
                      'readonly', 'branchUnaffecting'}
# branchUnaffecting can be readable and writeable, but the variable cannot affect, directly or indirectly,
# the sequence of evenat-states. We might later change this keyword to "validationOnly".

INFIX_FN_SYMBOLS = {'+', '-', '/', '*', '=='}
PREFIX_FN_SYMBOLS = {'append', 'removeOne', 'containedIn', 'get'
                     'contractStartDay', 'event_start_date' }



DEONTIC_MODALITIES = {'must','may','should'}
DeonticModality = str
DEONTIC_GUARD_MODALITIES = {'mustif','mayif','shouldif'}
DEONTIC_KEYWORDS = DEONTIC_GUARD_MODALITIES.union(DEONTIC_MODALITIES)
DeonticKeyword = str

DEADLINE_OPERATORS = {'by', 'within', 'on' ,'at', 'before', 'between'}
DEADLINE_KEYWORDS = {'immediately', 'nodeadline'}
DURATION_SYMBOLS = {'discretionary'}

# These are not actually case sensitive
GLOBAL_VARS_SECTION_LABEL = "GlobalVars"
CLAIMS_SECTION_LABEL = "Claims"
PROSE_CONTRACT_SECTION_LABEL = "ProseContract"
FORMAL_CONTRACT_SECTION_LABEL = "FormalContract"
ACTORS_SECTION_LABEL = "Actors"
DOT_FILE_NAME_LABEL = "DotFileName"
IMG_FILE_NAME_LABEL = "ImgFileName"


CONTRACT_PARAMETERS_SECTION_LABEL = "ContractParams"
EVENT_STATES_SECTION_LABEL = "EventStates"
START_STATE_LABEL = "StartState"


CODE_BLOCK_LABEL = "Entrance"
NONACTION_BLOCK_LABEL = "Fallbacks"
EVENT_STATE_DESCRIPTION_LABEL = "Description"
EVENT_STATE_PROSE_REFS_LABEL = "ProseRefs"
EVENT_STATE_ACTOR_BLOCKS_LABEL = "ActorEvents"
FULFILLED_EVENT_STATE_LABEL = "Fulfilled"

