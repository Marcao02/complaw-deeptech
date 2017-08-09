from typing import Any, List, Callable, Iterable


def streqci(s1:Any,s2:Any) -> bool:
    # ci for case insensitive
    return isinstance(s1,str) and isinstance(s2,str) and s1.lower() == s2.lower()


def is_singleton_string_list(lst:List[Any]) -> bool:
    return len(lst) == 1 and isinstance(lst[0],str)


def list_split(by:str, lst:List[str]) -> List[List[str]]:
    lists = []
    next : List[str] = []
    for x in lst:
        if x == by:
            lists.append(next)
            next = []
        else:
            next.append(x)
    if len(next) > 0:
        lists.append(next)
    return lists


def mapjoin(f:Callable[[Any],str], iter:Iterable[Any], delim:str) -> str:
    return delim.join(map(f,iter))