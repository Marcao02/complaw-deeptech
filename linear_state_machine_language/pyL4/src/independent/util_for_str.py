from typing import Any, Callable, Union, Iterable, Iterator


def strwbar(s:str,barsymb:str='-') -> str:
    return s + "\n" + barsymb*len(s)

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

