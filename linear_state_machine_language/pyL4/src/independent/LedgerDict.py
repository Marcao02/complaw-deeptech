from typing import Generic

from src.independent.typing_imports import *

K = TypeVar('K')
V = TypeVar('V')

LazyRecTuple = Optional[Tuple[Tuple[K,V],Any]]

class LedgerDict(Generic[K, V], Iterable):
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

    def set(self,k:K,v:Optional[V]) -> 'LedgerDict':
        if v is None:
            newkeys = self.keys.difference({v})
        else:
            newkeys = self.keys if (k in self.keys) else self.keys.union((k,))
        newchanges = ((k,v),self.changes)
        return LedgerDict._newLedgerDict(newchanges, newkeys)

    @staticmethod
    def _pruned(changes:LazyRecTuple, seen:Set[K]) -> LazyRecTuple:
        if changes is None:
            return None
        else:
            (key,val), rest = changes[0], changes[1]
            if key not in seen:
                seen.add(key)
                return ((key,val), LedgerDict._pruned(rest, seen))
            else:
                return LedgerDict._pruned(rest, seen)

    def __str__(self) -> str:
        if self.changes:
            # self.prune()
            return str(self._pruned(self.changes, set()))
        else:
            return "empty LedgerDict"

    def __contains__(self, item) -> bool:
        return item in self.keys

    def __getitem__(self,k:K) -> V:
        if not k in self.keys:
            raise KeyError
        else:
            return cast(V, LedgerDict.get_no_key_test(self.changes,k))

    @staticmethod
    def get_no_key_test(changes: LazyRecTuple, k: K) -> Optional[V]:
        if not changes or len(changes) == 0:
            return None
        elif changes[0][0] == k:
            return cast(V, changes[0][1])
        else:
            return LedgerDict.get_no_key_test(changes[1], k)

    def getmaybe(self,k:K) -> Optional[V]:
        if not k in self.keys:
            return None
        else:
            return LedgerDict.get_no_key_test(self.changes,k)

    def __iter__(self):
        return self.keys.__iter__()

