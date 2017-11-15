from typing import Any, List, Callable, Iterable, cast, Dict

def indent(i:int) -> str:
    return 4*i*' '

def caststr(x:Any) -> str:
    assert isinstance(x,str), x
    return cast(str,x)

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

def isFloat(x:str) -> bool:
    try:
        y = float(x)
        return True
    except ValueError:
        return False

def isInt(x:str) -> bool:
    try:
        y = int(x)
        return True
    except ValueError:
        return False

def hasNotNone(d:Dict[str,Any], x:str) -> bool:
    return (x in d) and not(d[x] is None)

def dictSetOrInc(d:Dict[str,int], key:str, init:int ) -> None:
    if key in d:
        d[key] += 1
    else:
        d[key] = init