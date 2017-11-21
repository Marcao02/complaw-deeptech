from typing import List, Union, Optional, NamedTuple

from model.Term import Term
from model.constants_and_defined_types import GlobalVarId


class GlobalVarDec(NamedTuple):
    name: GlobalVarId
    sort: str
    initval: Optional[Term]
    modifier:List[str]

    def __str__(self) -> str:
        return (' '.join(self.modifier) + ' ' if self.modifier else '') + \
               self.name + " : " + self.sort + \
               (" := " + str(self.initval) if (self.initval is not None) else '')

    def isWriteOnceMore(self) -> bool:
        return 'writeOnceMore' in self.modifier
