# from enum import Enum
from typing import Dict, NewType, cast

SortId = NewType('SortId', str)
SectionId = NewType('SectionId',str)
ActionId = NewType('ActionId',str)
RoleId = NewType('RoleId',str)
ProseClauseId = NewType('ProseClauseId',str)
GlobalVarId = NewType('GlobalVarId',str)
LocalVarId = NewType('LocalVarId',str)
LocalOrGlobalVarId = NewType('LocalOrGlobalVarId',str)
ActionParamId = NewType('ActionParamId',str)
ContractParamId = NewType('ContractParamId',str)
ConnectionActionParamId = NewType('ConnectionActionParamId',str)

ProseContract = Dict[ProseClauseId,str]
ParamsDec = Dict[ActionParamId, SortId]

SPECIAL_CONSTANTS = {'MAX_TIME', 'MAX_EVENT_STATE_CHANGES'}
VARIABLE_MODIFIERS = {'writeonce', 'writeonly', 'writeAtMostOnce',
                      'writeOnceMore', 'inconly', 'deconly',
                      'readonly', 'branchUnaffecting',
                      'reactive', 'nonoperative'}
# branchUnaffecting can be readable and writeable, but the variable cannot affect, directly or indirectly,
# the sequence of evenat-states. We might later change this keyword to "validationOnly".

DEADLINE_PREDICATES = { # THESE MUST ALL BE PREFIX CURRENTLY
                      'by', 'strictly-within', 'on' ,'at-ts', 'nonstrictly-before', 'between', 'after',
                      'strictly-before', 'nonstrictly-between-or-on','nonstrictly-after-ts-and-within', 'after-exactly',
                      'within', 'nonstrictly-after-ts', 'nonstrictly-within', 'after-exact-duration'}
DEADLINE_OPERATORS = { # THESE MUST ALL BE PREFIX CURRENTLY
                      'dateFromDayAndMonthIndices', 'nextMonthIndex' }
DEADLINE_KEYWORDS = {'immediately', 'nodeadline','discretionary'}

INFIX_FN_SYMBOLS = {'+', '-', '/', '*', '==', '≤', '≥', '<', '>',
                    'or','and', 'unitsAfter'}
PREFIX_FN_SYMBOLS = {'contractStartTimestamp', 'eventTimestamp', 'monthStartDay', 'monthEndDay',
                     'days', #'earliest',
                     'dateplus',
                     'ifthenelse',
                     'max', 'ceil', 'even', 'odd',
                     'not',
                     'enqueue', 'dequeue', 'discardTop', 'top',  # queues
                     'append', 'removeOne', 'containedIn', 'get', 'nonempty',# lists
                     'setAdd', 'setRemove', # sets
                     'tuple', 'fst', 'snd',
                     'entranceTimeNoLaterThan-ts?', 'entranceTimeAfter-ts?'

                     }.union(DEADLINE_OPERATORS, DEADLINE_PREDICATES)

POSTFIX_FN_SYMBOLS = {'unitsAfterEntrance'}

DEONTIC_MODALITIES = {'must','may','should','weakly-must'}
DeonticModality = NewType('DeonticModality',str)
# DEONTIC_GUARD_MODALITIES = {'mustif','mayif','shouldif'}
# DEONTIC_KEYWORDS = DEONTIC_GUARD_MODALITIES.union(DEONTIC_MODALITIES)


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

ENV_ROLE = cast(RoleId,"Env")

TRANSITIONS_TO_LABEL = "TransitionsTo"
CODE_BLOCK_LABEL = "StateTransform"
SECTION_DESCRIPTION_LABEL = "Description"
ACTION_DESCRIPTION_LABEL = "Description"
PROSE_REFS_LABEL = "ProseRefs"
OUT_CONNECTIONS_LABEL = "Next"

FULFILLED_SECTION_LABEL = cast(SectionId, "Fulfilled")


