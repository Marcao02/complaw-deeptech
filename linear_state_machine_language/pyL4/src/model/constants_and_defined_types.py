# from enum import Enum

from typing import Dict, NewType, cast, Union, List, Any

Nat = NewType('Nat',int)
# TimeInt = NewType('TimeInt',Nat)

# Data = Union[int, float, TimeInt] # later maybe Tuple[Data] and str
Data = Any # later maybe Tuple[Data] and str
TimeInt = int

RoleId = NewType('RoleId',str)
ProseClauseId = NewType('ProseClauseId',str)
GlobalVarId = NewType('GlobalVarId',str)
ActionId = NewType('ActionId',str)
SortId = NewType('SortId', str)
SectionId = NewType('SectionId',str)
DefinitionId = NewType('DefinitionId', str)
ContractParamId = NewType('ContractParamId',str)
ActionBoundActionParamId = NewType('ActionBoundActionParamId',str)
RuleBoundActionParamId = NewType('RuleBoundActionParamId',str)
# LocalVarId = NewType('LocalVarId',str)

ProseContract = Dict[ProseClauseId,str]
ParamsDec = Dict[ActionBoundActionParamId, SortId]

ContractParamSubst = Dict[ContractParamId,Data]
GVarSubst = Dict[GlobalVarId,Data]
ABAPNamedSubst = Dict[ActionBoundActionParamId, Data]
RBAPNamedSubst = Dict[RuleBoundActionParamId, Data]
AParamsSubst = List[Data]
ABAPSubst = List[Data]
RBAPSubst = List[Data]

SPECIAL_CONSTANTS = {'MAX_TIME', 'MAX_EVENT_STATE_CHANGES'}
VARIABLE_MODIFIERS = {'writeonce', 'writeonly', 'writeAtMostOnce',
                      'writeOnceMore', 'inconly', 'deconly',
                      'readonly', 'branchUnaffecting',
                      'reactive', 'nonoperative'}
# branchUnaffecting can be readable and writeable, but the variable cannot affect, directly or indirectly,
# the sequence of event-states. We might later change this keyword to "validationOnly".

PREFIX_DEADLINE_PREDICATES = {
                      'by', 'strictly-within', 'on-ts' ,'at-ts', 'nonstrictly-before', 'between', 'after',
                      'strictly-before', 'nonstrictly-between-or-on','nonstrictly-after-td-and-within',
                      'after-exactly', # duration
                      'within', #duration
                      'strictly-after', 'nonstrictly-after-td', 'nonstrictly-within', 'after-exact-duration',
                        'sectionEntranceTimedelta'
                      }
DEADLINE_PREDICATES = PREFIX_DEADLINE_PREDICATES.union({'≤','≥','<','>'})

DEADLINE_OPERATORS = { # THESE MUST ALL BE PREFIX CURRENTLY
                      'dateFromDayAndMonthIndices', 'nextMonthIndex', 'event_td',
                        'monthStartDay', 'monthEndDay'}
DEADLINE_KEYWORDS = {'immediately', 'no_time_constraint','discretionary'}

CONTRACT_VALUE_PROPERTIES = {'MAX_TIME', 'MAX_SECTION_VISITS'}

EXEC_ENV_VARIABLES = {'contractStartDatetime', 'contractStartTimedelta',
                      'sectionEntranceTimedelta', 'event_td'}

PREFIX_FN_SYMBOLS = {'entranceTimeNoLaterThan-ts?',
                     'entranceTimeAfter-ts?',
                     'days', 'earliest',
                     'ifthenelse',
                     'max', 'min', 'ceil', 'even', 'odd', 'round',
                     'and*', 'not',
                     'enqueue', 'dequeue', 'discardTop', 'top',  # queues
                     'append', 'removeOne', 'containedIn', 'get', 'nonempty',# lists
                     'setAdd', 'setRemove', # sets
                     'tuple', 'fst', 'snd'
                     }.union(DEADLINE_OPERATORS, PREFIX_DEADLINE_PREDICATES)
INFIX_FN_SYMBOLS = {'+', '-', '/', '*', '==', '≤', '≥', '<', '>',
                    'or','and', 'unitsAfter'}

POSTFIX_FN_SYMBOLS = {'unitsAfterEntrance'}

DEONTIC_KEYWORDS = {'must','may','should','weakly-must','may-later','must-later'}
DeonticKeyword = NewType('DeonticKeyword',str)




# Aside form "Misc" group, the following are not actually case sensitive

# ------------Top-level declarations------------
GLOBAL_VARS_AREA_LABEL = "GlobalVars"
PROSE_CONTRACT_AREA_LABEL = "ProseContract"
FORMAL_CONTRACT_AREA_LABEL = "FormalContract"
ROLES_DEC_LABEL = "Roles"
TIMEUNIT_DEC_LABEL = "TimeUnit"
SUPPORTED_TIMEUNITS = ['w','d','h','m','s']
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
LOOP_KEYWORD = "SRC"
CODE_BLOCK_LABEL = "StateTransform" # Or "EntranceTransform"
ACTION_DESCRIPTION_LABEL = "Description"
ACTION_PRECONDITION_LABEL = "Pre"
ACTION_POSTCONDITION_LABEL = "Post"
FOLLOWING_SECTION_DEC_LABEL = "FollowingSection"


# ------------Misc------------
FULFILLED_SECTION_LABEL = cast(SectionId, "Fulfilled")
# CONTINGENT_FULFILLED_SECTION_LABEL = cast(SectionId, "ContingentFulfilled")
ENV_ROLE = cast(RoleId,"Env")