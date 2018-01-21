from src.model.Sort import Sort
from src.typesystem.standard_sorts import TEMP_SORT_IDENTIFICATION


def temp_normalize_sort(s: str) -> Sort:
    if s in TEMP_SORT_IDENTIFICATION:
        return TEMP_SORT_IDENTIFICATION[s]
    else:
        return s
