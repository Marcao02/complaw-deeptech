from typing import Optional


class FileCoord:
    def __init__(self, line:int, col:Optional[int]) -> None:
        self.line = line
        self.col = col