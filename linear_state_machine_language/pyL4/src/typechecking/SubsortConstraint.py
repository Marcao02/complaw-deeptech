from src.independent.typing_imports import *

from src.model.Sort import Sort

class SubsortConstraint(NamedTuple):
    parts: Tuple[Sort,Sort]
    # def complexity(self) -> int:
    #     return max(self.parts[0].complexity(), self.parts[1].complexity())

# class ParamSubsortConstraint(NamedTuple):
#     parts: Tuple[Sort,Sort]
    

# SubSort chain
def sschain(*sorts:Sort) -> List[SubsortConstraint]:
    return [ SubsortConstraint( (sorts[i],sorts[i+1]) ) for i in range(len(sorts)-1)]

