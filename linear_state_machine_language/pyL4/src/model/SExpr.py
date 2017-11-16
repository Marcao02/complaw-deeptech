# v 1.1.0

import logging
from typing import Union, List, Any, TypeVar, Sequence, cast, Sized, Iterable
from model.util import is_singleton_string_list, caststr

print(caststr)

STRING_LITERAL_MARKER = "STRLIT"
COMMENT_LITERAL_MARKER = "COMMENT"
LINE_COMMENT_START_CHAR = ';'

left_groupers = {'(','{','[','‹','❪'}
right_groupers = {')','}',']','›','❫'}
grouper_map = {'(':')', '{':'}', '[':']', '‹':'›', '❪':'❫', '"':'"', "'":"'", '`':'`', LINE_COMMENT_START_CHAR:'\n'}
quotelike = {"'",'"','`'}
double_splits_word_only = {':=','+=','-=','*=','==','<=', '>=', '->', '=>', '<-'}
splits_word_only = {':','=',','}
all_symb_tags = quotelike.union(left_groupers).union(';')


"""
Formerly was a subtype of List[Union['SExpr', str]], but that exposed too many List methods, making bugs harder to find.
Now a wrapper around a list of SExpr's and strings.
"""
class SExpr(Sized,Iterable): #(List[Union['SExpr', str]]):
    def __init__(self,
                 symb:str, # member of left_groupers
                 lst:List[Union['SExpr', str]] = None,
                 line:int = None,
                 col:int = None) -> None:
        # super().__init__(lst if lst else [])
        self.symb = symb
        self.lst = lst if lst else []
        self.line = line
        self.col = col
        assert isinstance(symb,str) and symb in all_symb_tags

    def tillEnd(self,i:int) -> 'SExpr':
        return SExpr(self.symb, self.lst[i:], self.line, self.col)

    def fromStartToExclusive(self,i:int) -> 'SExpr':
        return SExpr(self.symb, self.lst[:i], self.line, self.col)

    def withDropped(self,i:int) -> 'SExpr':
        return SExpr(self.symb, self.lst[:i] + self.lst[i+1:], self.line, self.col)

    def __getitem__(self, item):
        return self.lst.__getitem__(item)

    def __len__(self):
        return len(self.lst)

    def __iter__(self):
        return self.lst.__iter__()

    def append(self, x:Union['SExpr',str]):
        self.lst.append(x)

    def __str__(self):
        return str(self.lst)

    def __repr__(self):
        return repr(self.lst)

def castse(x: Any) -> SExpr:
    # assert isinstance(x, list), x
    return cast(SExpr, x)

SExprOrStr = Union[SExpr,str]
