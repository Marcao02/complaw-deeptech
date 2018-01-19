from typing import NamedTuple, Optional, TYPE_CHECKING


if TYPE_CHECKING:
    from src.typesystem.FnTypes import OverloadedFnType

class FnSymb(NamedTuple):
    name: str
    type: Optional['OverloadedFnType']