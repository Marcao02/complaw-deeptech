from typing import Any, List, Dict, Union, Sequence, TypeVar, Set, Iterable, Tuple

K = TypeVar('K')
V = TypeVar('V')


def gather_to_map_to_sets(pairs:Iterable[Tuple[K,V]]) -> Dict[K,Set[V]]:
    rv : Dict[K,Set[V]] = dict()
    for k,v in pairs:
        if k in rv:
            rv[k].add(v)
        else:
            rv[k] = {v}
    return rv

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

def nested_list_replace(lst_or_str:Union[str,Sequence[Any]], old:str, new:str) -> Any:
    if isinstance(lst_or_str,str):
        return new if lst_or_str == old else lst_or_str
    else:
        return tuple(nested_list_replace(part, old, new) for part in lst_or_str)

def nested_list_replace_mult( lst_or_str:Union[str,Sequence[Any]],
                              subst:Dict[str,Any] ) -> Any:
    if isinstance(lst_or_str,str):
        return subst[lst_or_str] if lst_or_str in subst else lst_or_str
    else:
        return tuple(nested_list_replace_mult(part, subst) for part in lst_or_str)
