from src.independent.typing_imports import *

V = TypeVar('V')

TupleLinkedList = Optional[Tuple[V, Any]]

def tllToStr(tll: TupleLinkedList) -> str:
    if tll is None:
        return "‹empty›"
    else:
        return f"{tll[0]} -> {tllToStr(tll[1])}"

def count(tll: TupleLinkedList, v: V) -> int:
    if tll is None:
        return 0
    else:
        if len(tll) < 2:
            print("tll:", tll)
        return (1 if tll[0] == v else 0) + count(tll[1], v)

def toList(tll: TupleLinkedList) -> List[V]:
    rv : List[V] = []
    def _helper(_tll:TupleLinkedList):
        if _tll is not None:
            rv.append(_tll[0])
            _helper(_tll[1])
    _helper(tll)
    return rv

