from typing import Any, Callable, Iterable, cast, Dict, Set, TypeVar, Type, Iterator, Union
import logging

import time


def mytimeit(f:Callable[[],Any]) -> Any:
    start = time.process_time()
    rv = f()
    print("TIME", 10 * (time.process_time() - start))
    return rv

def indent(i:int) -> str:
    return 4*i*' '

def caststr(x:Any) -> str:
    assert isinstance(x,str), x
    return x

T = TypeVar('T')
def chcast(tp:Type[T],x:Any) -> T:
    assert isinstance(x,tp), f"{str(x)} is a {type(x)} but this chcast requires a {tp}."
    return x

def chcaststr(x:Any) -> str:
    assert isinstance(x,str), f"{str(x)} is a {type(x)} but this chcast requires a str."
    return x


def castid(tp:Type[T],x:Any) -> T:
    assert isinstance(x,str), f"{str(x)} is a {type(x)} but this chcast requires a {tp}."
    return cast(T,x)

def streqci(s1:Any,s2:Any) -> bool:
    # ci for case insensitive
    return isinstance(s1,str) and isinstance(s2,str) and s1.lower() == s2.lower()


def mapjoin(f:Callable[[Any],str], iter:Union[Iterable[Any],Iterator[Any]], delim:str='') -> str:
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
    except Exception:
        return False

S = TypeVar('S',bound=str)

def hasNotNone(d:Dict[S,Any], x:S) -> bool:
    return (x in d) and not(d[x] is None)

warnings_given : Set[str] = set()
def warn_once(msg) -> None:
    if not msg in warnings_given:
        logging.warning(msg)
        warnings_given.add(msg)

PRINT_TODOS_THROUGHOUT = False
todos_given : Set[str] = set()
def todo_once(msg) -> None:
    if not msg in todos_given:
        if PRINT_TODOS_THROUGHOUT:
            print("TODO: " + msg)
        todos_given.add(msg)
def print_all_todos() -> None:
    for todo in todos_given:
        print("TODO: " + todo)

class ContractBug(Exception):
    pass
def contract_bug(msg:str) -> None:
    raise ContractBug(msg)