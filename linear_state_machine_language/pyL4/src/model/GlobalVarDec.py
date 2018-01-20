from typing import List, Optional, NamedTuple

from src.constants_and_defined_types import GlobalVarId
from src.model.Term import Term
from src.typesystem.Sorts import Sort


class GlobalVarDec(NamedTuple):
    name: GlobalVarId
    sort: Sort
    initval: Optional[Term]
    modifier:List[str]

    def __str__(self) -> str:
        return (' '.join(self.modifier) + ' ' if self.modifier else '') + \
               self.name + " : " + str(self.sort) + \
               (" := " + str(self.initval) if (self.initval is not None) else '')

    def isWriteOnceMore(self) -> bool:
        return 'writeOnceMore' in self.modifier
