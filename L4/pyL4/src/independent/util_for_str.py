from typing import Any, Callable, Union, Iterable, Iterator, Sequence, List


def strwbar(s:str,barsymb:str='-') -> str:
    return s + "\n" + barsymb*len(s)

def streqci(s1:Any,s2:Any) -> bool:
    # ci for case insensitive
    return isinstance(s1,str) and isinstance(s2,str) and s1.lower() == s2.lower()


def mapjoin(f:Callable[[Any],str],
            iter:Union[Iterable[Any],Iterator[Any]],
            delim:str='') -> str:
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


def nonemptySortedSubsets( lst:Sequence[str] ) -> List[List[str]]:

    def helper(_lst:List[str]) -> List[List[str]]:
        if len(_lst) == 0:
            return [[]]
        else:
            subprob = helper(_lst[1:])
            rv = subprob.copy()
            for s in subprob:
                rv.append([_lst[0]] + s)
            return rv

    lst = sorted(lst)
    rv = helper(lst)[1:] # first element is empty list
    return rv