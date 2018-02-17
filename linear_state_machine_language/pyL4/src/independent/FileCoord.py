from typing import Optional, NamedTuple


class FileCoord(NamedTuple):
    line: int
    col: Optional[int]

    def __str__(self) -> str:
        if self.col is None:
            return f"l{self.line}"
        else:
            return f"l{self.line}:c{self.col}"