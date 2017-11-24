from typing import Any, List, Callable, Iterable, cast, Dict, Set, TypeVar, Type
import logging

def indent(i:int) -> str:
    return 4*i*' '

def caststr(x:Any) -> str:
    assert isinstance(x,str), x
    return cast(str,x)

T = TypeVar('T')
def chcast(tp:Type[T],x:Any) -> T:
    assert isinstance(x,tp), f"{str(x)} is a {type(x)} but this chcast requires a {tp}."
    return cast(T,x)

def chcaststr(x:Any) -> str:
    assert isinstance(x,str), f"{str(x)} is a {type(x)} but this chcast requires a str."
    return cast(str,x)

def castid(tp:Type[T],x:Any) -> T:
    assert isinstance(x,str), f"{str(x)} is a {type(x)} but this chcast requires a {tp}."
    return cast(T,x)

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


def mapjoin(f:Callable[[Any],str], iter:Iterable[Any], delim:str='') -> str:
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

S = TypeVar('S',bound=str)

def hasNotNone(d:Dict[S,Any], x:S) -> bool:
    return (x in d) and not(d[x] is None)

def dictSetOrInc(d:Dict[S,int], key:S, init:int ) -> None:
    if key in d:
        d[key] += 1
    else:
        d[key] = init

def writeFile(path:str, contents:str) -> None:
    f = open(path, 'w', encoding='utf8')
    f.write(contents)
    f.close()

def writeReadOnlyFile(path:str, contents:str) -> None:
    from os import system
    system(f'chmod u+w {path}')
    writeFile(path, contents)
    system(f'chmod a-w {path}')

warnings_given : Set[str] = set()
def warn_once(msg) -> None:
    if not msg in warnings_given:
        logging.warning(msg)
        warnings_given.add(msg)

todos_given : Set[str] = set()
def todo_once(msg) -> None:
    if not msg in todos_given:
        print("TODO: " + msg)
        todos_given.add(msg)

class ContractBug(Exception):
    pass
def contract_bug(msg:str) -> None:
    raise ContractBug(msg)