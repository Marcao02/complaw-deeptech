from src.independent.typing_imports import *

from src.model.Sort import Sort

class SubsortConstraint(NamedTuple):
    parts: Tuple[Sort,Sort]

# class ParamSubsortConstraint:
    

# SubSort chain
def sschain(*sorts:Sort) -> List[SubsortConstraint]:
    return [ SubsortConstraint( (sorts[i],sorts[i+1]) ) for i in range(len(sorts)-1)]

