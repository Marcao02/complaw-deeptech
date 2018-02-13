# from enum import Enum
from typing import Dict, NewType, cast, Union, List, Any, Set, TYPE_CHECKING

if TYPE_CHECKING:
    from src.model.Sort import Sort

    # from Sort import Sort
# Data = Union[int, float, TimeInt] # later maybe Tuple[Data] and str
Data = Any # later maybe Tuple[Data] and str

RoleId = NewType('RoleId',str)
ProseClauseId = NewType('ProseClauseId',str)

StateVarId = NewType('StateVarId', str)
LocalVarId = NewType('LocalVarId', str)
ActionId = NewType('ActionId',str)
SortId = NewType('SortId', str)
SectionId = NewType('SectionId',str)
DefinitionId = NewType('DefinitionId', str)
ContractParamId = NewType('ContractParamId',str)
ActionBoundActionParamId = NewType('ActionBoundActionParamId',str)
RuleBoundActionParamId = NewType('RuleBoundActionParamId',str)

ProseContract = Dict[ProseClauseId,str]
ParamsDec = Dict[ActionBoundActionParamId, 'Sort']

ContractParamSubst = Dict[ContractParamId,Data]
GVarSubst = Dict[StateVarId, Data]
LocalVarSubst = Dict[LocalVarId, Data]
ABAPNamedSubst = Dict[ActionBoundActionParamId, Data]
RBAPNamedSubst = Dict[RuleBoundActionParamId, Data]
AParamsSubst = List[Data]
ABAPSubst = List[Data]
RBAPSubst = List[Data]

VARIABLE_MODIFIERS = {'writeonce', 'writeonly', 'writeatmostonce',
                      'writeoncemore', 'inconly', 'deconly',
                      'readonly', 'branchunaffecting',
                      'reactive', 'nonoperative'}
# branchUnaffecting can be readable and writeable, but the variable cannot affect, directly or indirectly,
# the sequence of event-states. We might later change this keyword to "validationOnly".

TIME_CONSTRAINT_PREDICATES = {'≤','≥','<','>','==','and','tdGEQ', 'tdLT'}

TIME_CONSTRAINT_OPERATORS = { # THESE MUST ALL BE PREFIX CURRENTLY
                      'dateFromDayAndMonthIndices', 'nextMonthIndex',
                        'monthStartDay_td', 'monthEndDay_td'}

TIME_CONSTRAINT_KEYWORDS = {'immediately', 'no_time_constraint','discretionary'}
# PRACTICALLY_FOREVER = timedelta(weeks=99999)

CONTRACT_VALUE_PROPERTIES = {'MAX_TIME', 'MAX_SECTION_VISITS'}

EXEC_ENV_VARIABLES = {'contractStart_dt',
                      'contractStart_td',
                      'sectionEntrance_td',
                      'event_td',
                      'next_event_dt',
                      'next_event_td',
                      'future_event_td',
                      'event_role'}

PREFIX_FN_SYMBOLS = { 'cast',
                      'units',

                      'days',
                      'str2datetime',

                     'ifthenelse',
                     'and', 'or', 'not',
                     'max', 'min',

                     'ceil', 'round',
                     'even', 'odd',

                      # derived, but useful for being rigorous about types
                     'fraction-of-sum',

                     'tuple', 'tupleGet',
                     'emptyTDMap', # should be a constant but more important things to do
                     'mapSet','mapDelete','mapHas','tdGEQ','tdLT',
                     'nonempty', 'empty'
                     }.union(TIME_CONSTRAINT_OPERATORS)

INFIX_FN_SYMBOLS = {'+', '-', '/', '*', '==', '≤', '≥', '<', '>', '^',
                    'or','and',
                    # derived, but useful for being rigorous about types
                    'floor/', 'round/', 'ceil/',
                    }

POSTFIX_FN_SYMBOLS : Set[str] = set()

DEONTIC_KEYWORDS = {'must','may','should','obligation-options-include','may-later','must-later'}
DeonticKeyword = NewType('DeonticKeyword',str)

# Aside form "Misc" group, the following are not actually case sensitive

# ------------Top-level declarations------------
GLOBAL_VARS_AREA_LABEL = "GlobalVars"
PROSE_CONTRACT_AREA_LABEL = "ProseContract"
FORMAL_CONTRACT_AREA_LABEL = "FormalContract"
ROLES_DEC_LABEL = "Roles"
TIMEUNIT_DEC_LABEL = "TimeUnit"
SUPPORTED_TIMEUNITS = ['w','d','h','m','s']
MACRO_DEC_LABEL = "Macro"
CONTRACT_PARAMETERS_AREA_LABEL = "ContractParams"
TOPLEVEL_CLAIMS_AREA_LABEL = "Claims"
TOPLEVEL_STATE_INVARIANTS_AREA_LABEL = "Invariants"
DEFINITIONS_AREA = "Definitions"
SORT_DEFINITIONS_AREA = "SortDefinitions"
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