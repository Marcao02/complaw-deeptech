from typing import Dict

Sort = str
SectionId = str
ActionId = str
RoleId = str
ProseClauseId = str
GlobalVarId = str

ProseContract = Dict[str,str]
ParamsDec = Dict[str,Sort]

SPECIAL_CONSTANTS = {'MAX_TIME', 'MAX_EVENT_STATE_CHANGES'}
VARIABLE_MODIFIERS = {'writeonce', 'writeonly', 'writeAtMostOnce',
                      'writeOnceMore', 'inconly', 'deconly',
                      'readonly', 'branchUnaffecting',
                      'reactive', 'nonoperative'}
# branchUnaffecting can be readable and writeable, but the variable cannot affect, directly or indirectly,
# the sequence of evenat-states. We might later change this keyword to "validationOnly".

DEADLINE_OPERATORS = { # THESE MUST ALL BE PREFIX CURRENTLY
                        'by', 'strictly-within', 'on' ,'at', 'nonstrictly-before', 'between', 'after',
                      'strictly-before', 'nonstrictly-between-or-on','nonstrictly-after-and-within', 'after-exactly',
                      'dateFromDayAndMonthIndices', 'nextMonthIndex', 'within', 'nonstrictly-after',
                      'nonstrictly-within'
                      }
DEADLINE_KEYWORDS = {'immediately', 'nodeadline'}
DURATION_SYMBOLS = {'discretionary'}

INFIX_FN_SYMBOLS = {'+', '-', '/', '*', '==', '≤', '≥', '<', '>',
                    'or','and', 'unitsAfter'}
PREFIX_FN_SYMBOLS = {'contract_start_date', 'event_start_date', 'event_start_time', 'monthStartDay', 'monthEndDay',
                     'days', #'earliest',
                     'dateplus',
                     'ifthenelse',
                     'max', 'ceil',
                     'not',
                     'enqueue', 'dequeue', 'discardTop', 'top',  # queues
                     'append', 'removeOne', 'containedIn', 'get', 'nonempty',# lists
                     'setAdd', 'setRemove', # sets
                     'tuple', 'fst', 'snd',
                     'currentTimeNoLaterThan?', 'currentTimeAfter?',
                     'unitsAfter', 'unitsAfterEntrance'
                     }.union(DEADLINE_OPERATORS)

POSTFIX_FN_SYMBOLS = {'unitsAfterEntrance'}



DEONTIC_MODALITIES = {'must','may','should','weakly-must'}
DeonticModality = str
# DEONTIC_GUARD_MODALITIES = {'mustif','mayif','shouldif'}
# DEONTIC_KEYWORDS = DEONTIC_GUARD_MODALITIES.union(DEONTIC_MODALITIES)
DeonticKeyword = str


# These are not actually case sensitive
GLOBAL_VARS_SECTION_LABEL = "GlobalVars"
CLAIMS_SECTION_LABEL = "Claims"
PROSE_CONTRACT_SECTION_LABEL = "ProseContract"
FORMAL_CONTRACT_SECTION_LABEL = "FormalContract"
ROLES_SECTION_LABEL = "Roles"
DOT_FILE_NAME_LABEL = "DotFileName"
IMG_FILE_NAME_LABEL = "ImgFileName"


CONTRACT_PARAMETERS_SECTION_LABEL = "ContractParams"
# EVENT_STATES_SECTION_LABEL = "Sections"
START_SECTION_LABEL = "StartSection"

SECTION_LABEL = "Section"
ACTION_LABEL = "Action"

ENV_ROLE = "Env"

TRANSITIONS_TO_LABEL = "TransitionsTo"
CODE_BLOCK_LABEL = "Entrance"
SECTION_DESCRIPTION_LABEL = "Description"
ACTION_DESCRIPTION_LABEL = "Description"
PROSE_REFS_LABEL = "ProseRefs"
OUT_CONNECTIONS_LABEL = "Next"

FULFILLED_SECTION_LABEL = "Fulfilled"


