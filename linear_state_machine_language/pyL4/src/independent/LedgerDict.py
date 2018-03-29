from typing import Generic

from src.independent.typing_imports import *

K = TypeVar('K')
V = TypeVar('V')

LazyRecTuple = Optional[Tuple[Tuple[K,V],Any]]

class LedgerDict(Generic[K, V]):
    """An append-only (thus maximally shareable), immutable, key value store"""

    # nested tuples, forming a list in reverse order
    changes : LazyRecTuple # the Any is being lazy with types
    keys: FrozenSet[K]

    def __init__(self, d:Dict[K,V]) -> None:
        self.keys = frozenset(d.keys())
        self.changes : LazyRecTuple = None
        for pair in d.items():
            self.changes = (pair, self.changes)

    @staticmethod
    def _newLedgerDict(changes : Tuple[Tuple[K,V],Any], keys: FrozenSet[K]) -> 'LedgerDict':
        rv : LedgerDict[K,V] = LedgerDict({})
        rv.changes = changes
        rv.keys = keys
        return rv

    def set(self,k:K,v:V) -> 'LedgerDict':
        newkeys = self.keys if (k in self.keys) else self.keys.union((k,))
        newchanges = ((k,v),self.changes)
        return LedgerDict._newLedgerDict(newchanges, newkeys)

    def __str__(self) -> str:
        if self.changes:
            return str(self.changes)
        else:
            return "empty LedgerDict"

    def __contains__(self, item) -> bool:
        return item in self.keys

    def __getitem__(self,k:K) -> Optional[V]:
        if not k in self.keys:
            return None
        else:
            return get_no_key_test(self.changes,k)

    def __iter__(self):
        return self.keys.__iter__()

def get_no_key_test(changes:LazyRecTuple, k:K) -> Optional[V]:
    if not changes or len(changes) == 0:
        return None
    elif changes[0][0] == k:
        return cast(V,changes[0][1])
    else:
        return get_no_key_test(changes[1],k)
