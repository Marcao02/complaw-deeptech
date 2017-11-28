# from enum import Enum
from typing import Dict, NewType, cast, Union, Tuple, Any

RoleId = NewType('RoleId',str)
ProseClauseId = NewType('ProseClauseId',str)
GlobalVarId = NewType('GlobalVarId',str)
ActionId = NewType('ActionId',str)
SortId = NewType('SortId', str)
SectionId = NewType('SectionId',str)
DefinitionId = NewType('DefinitionId', str)
ContractParamId = NewType('ContractParamId',str)

LocalVarId = NewType('LocalVarId',str)
LocalOrGlobalVarId = NewType('LocalOrGlobalVarId',str)
ActionParamId_BoundBy_ActionDecl = NewType('ActionParamId_BoundBy_ActionDecl',str)
ActionParamId_BoundBy_ActionRule = NewType('ActionParamId_BoundBy_ActionRule',str)

ProseContract = Dict[ProseClauseId,str]
ParamsDec = Dict[ActionParamId_BoundBy_ActionDecl, SortId]

Nat = NewType('Nat',int)
TimeStamp = NewType('TimeStamp',Nat)

ActionParamValue = Union[Tuple[Any], str, int, float, TimeStamp]
ActionParamSubst = Dict[ActionParamId_BoundBy_ActionDecl, ActionParamValue]

SPECIAL_CONSTANTS = {'MAX_TIME', 'MAX_EVENT_STATE_CHANGES'}
VARIABLE_MODIFIERS = {'writeonce', 'writeonly', 'writeAtMostOnce',
                      'writeOnceMore', 'inconly', 'deconly',
                      'readonly', 'branchUnaffecting',
                      'reactive', 'nonoperative'}
# branchUnaffecting can be readable and writeable, but the variable cannot affect, directly or indirectly,
# the sequence of evenat-states. We might later change this keyword to "validationOnly".

PREFIX_DEADLINE_PREDICATES = {
                      'by', 'strictly-within', 'on-ts' ,'at-ts', 'nonstrictly-before', 'between', 'after',
                      'strictly-before', 'nonstrictly-between-or-on','nonstrictly-after-ts-and-within',
                      'after-exactly', # duration
                      'within', #duration
                      'strictly-after', 'nonstrictly-after-ts', 'nonstrictly-within', 'after-exact-duration'
                      }
DEADLINE_PREDICATES = PREFIX_DEADLINE_PREDICATES.union({'≤','≥','<','>'})

DEADLINE_OPERATORS = { # THESE MUST ALL BE PREFIX CURRENTLY
                      'dateFromDayAndMonthIndices', 'nextMonthIndex' }
DEADLINE_KEYWORDS = {'immediately', 'nodeadline','discretionary'}

CONTRACT_VALUE_PROPERTIES = {'MAX_TIME', 'MAX_SECTION_VISITS'}

EXEC_ENV_VARIABLES = {'contractStartTimestamp', 'sectionEntranceTimestamp', 'event_ts'}

INFIX_FN_SYMBOLS = {'+', '-', '/', '*', '==', '≤', '≥', '<', '>',
                    'or','and', 'unitsAfter'}
PREFIX_FN_SYMBOLS = {'contractStartTimestamp', 'sectionEntranceTimestamp', 'eventTimestamp', 'monthStartDay', 'monthEndDay',
                     'days', 'earliest',
                     'dateplus',
                     'ifthenelse',
                     'max', 'min', 'ceil', 'even', 'odd', 'round',
                     'not',
                     'enqueue', 'dequeue', 'discardTop', 'top',  # queues
                     'append', 'removeOne', 'containedIn', 'get', 'nonempty',# lists
                     'setAdd', 'setRemove', # sets
                     'tuple', 'fst', 'snd',
                     'entranceTimeNoLaterThan-ts?', 'entranceTimeAfter-ts?'

                     }.union(DEADLINE_OPERATORS, PREFIX_DEADLINE_PREDICATES)

POSTFIX_FN_SYMBOLS = {'unitsAfterEntrance'}

DEONTIC_MODALITIES = {'must','may','should','weakly-must'}
DeonticModality = NewType('DeonticModality',str)




# Aside form "Misc" group, the following are not actually case sensitive

# ------------Top-level declarations------------
GLOBAL_VARS_AREA_LABEL = "GlobalVars"
PROSE_CONTRACT_AREA_LABEL = "ProseContract"
FORMAL_CONTRACT_AREA_LABEL = "FormalContract"
ROLES_DEC_LABEL = "Roles"
TIME_UNIT_DEC_LABEL = "TimeUnit"
STR_ARG_MACRO_DEC_LABEL = "Macro"
CONTRACT_PARAMETERS_AREA_LABEL = "ContractParams"
TOPLEVEL_CLAIMS_AREA_LABEL = "Claims"
DEFINITIONS_AREA = "Definitions"
DOT_FILE_NAME_LABEL = "DotFileName"
IMG_FILE_NAME_LABEL = "ImgFileName"

# ------------Inside main formal contract declarations------------
APPLY_MACRO_LABEL = "apply-macro"
START_SECTION_LABEL = "StartSection"
SECTION_LABEL = "Section"
ACTION_LABEL = "Action"

# ------------Inside a section declaration------------
ALLOWED_SUBJECTS_DEC_LABEL = "AllowedSubjects"
SECTION_DESCRIPTION_LABEL = "Description"
SECTION_PRECONDITION_LABEL = "Pre"
PROSE_REFS_LABEL = "ProseRefs"
OUT_CONNECTIONS_LABEL = "Next"

# ------------Inside an action declaration------------
TRANSITIONS_TO_LABEL = "TransitionsTo"
CODE_BLOCK_LABEL = "StateTransform"
ACTION_DESCRIPTION_LABEL = "Description"
ACTION_PRECONDITION_LABEL = "Pre"
ACTION_POSTCONDITION_LABEL = "Post"
FOLLOWING_SECTION_DEC_LABEL = "FollowingSection"


# ------------Misc------------
FULFILLED_SECTION_LABEL = cast(SectionId, "Fulfilled")
ENV_ROLE = cast(RoleId,"Env")