from typing import Optional


class FileCoord:
    def __init__(self, line:int, col:Optional[int]) -> None:
        self.line = line
        self.col = col

    def __str__(self) -> str:
        if self.col is None:
            return f"l{self.line}"
        else:
            return f"l{self.line}:c{self.col}"