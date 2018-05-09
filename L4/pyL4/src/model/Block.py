from src.model.Statement import Statement, IfElse
from src.independent.typing_imports import *

class Block:
    stmts: List[Statement]
    parent_ifelse: Optional[IfElse]