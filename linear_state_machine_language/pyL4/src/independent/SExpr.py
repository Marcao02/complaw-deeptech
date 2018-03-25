# v 2.0.0

from typing import Union, List, Any, cast, Sized, Iterable, Sequence, Callable

from src.independent.FileCoord import FileCoord
from src.independent.util import chcaststr

STRING_LITERAL_MARKER = "STRLIT"
COMMENT_LITERAL_MARKER = "COMMENT"
LINE_COMMENT_START_CHAR = ';'

left_groupers = {'(','{','[','‹','❪'}
right_groupers = {')','}',']','›','❫'}
grouper_map = {'(':')', '{':'}', '[':']', '‹':'›', '❪':'❫', '"':'"', "'":"'", '`':'`', LINE_COMMENT_START_CHAR:'\n'}
quotelike = {"'",'"','`'}

double_splits_word_only = {':=','+=','-=','*=','==','<=', '>=', '->', '=>', '<-'}
splits_word_only = {':','=',','}

all_symb_tags = quotelike.union(left_groupers).union(LINE_COMMENT_START_CHAR)


"""
Formerly was a subtype of List[Union['SExpr', str]], but that exposed too many List methods, making bugs harder to find.
Now a wrapper around a list of SExpr's and strings.
"""
class SExpr(Sized,Iterable): #(List[Union['SExpr', str]]):
    def __init__(self, lst: List[Union['SExpr', str]], line: int, col: int, symb: str = "(") -> None:
        self.symb = symb # member of left_groupers... always?
        self.lst = lst if lst else []
        self.line = line
        self.col = col
        assert isinstance(symb,str) and symb in all_symb_tags

    def coord(self) -> FileCoord:
        return FileCoord(self.line, self.col)

    def tillEnd(self,i:int) -> 'SExpr':
        return SExpr(self.lst[i:], self.line, self.col, self.symb)

    def fromStartToExclusive(self,i:int) -> 'SExpr':
        return SExpr(self.lst[:i], self.line, self.col, self.symb)

    def withElementDropped(self,i:int) -> 'SExpr':
        return SExpr(self.lst[:i] + self.lst[i + 1:], self.line, self.col, self.symb)

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

# old single-parameter version
# def sexpr_subst_string(sexpr_or_str: SExprOrStr, str_to_replace: str, replacement_str: str) -> SExprOrStr:
#     if isinstance(sexpr_or_str, str):
#         return sexpr_or_str.replace(str_to_replace, replacement_str)
#     else:
#         return SExpr(
#             symb = sexpr_or_str.symb.replace(str_to_replace, replacement_str),
#             lst = list(map(lambda child: sexpr_subst_string(child, str_to_replace, replacement_str), sexpr_or_str.lst)),
#             line = sexpr_or_str.line,
#             col = sexpr_or_str.col
#         )

# THIS ONLY MAKES SENSE FOR MACROS
def mult_replace(s:str, olds:Sequence[str], news:Sequence[SExprOrStr]) -> SExprOrStr:
    assert len(olds) == len(news)
    rv = s
    for i in range(len(olds)):
        if isinstance(news[i],str):
            rv = rv.replace(olds[i], chcaststr(news[i]))
        elif s == olds[i]: # if a string is exactly the parameter name, only in that case can we substitute in an SExpr
            return news[i]

    return rv

# THIS ONLY MAKES SENSE FOR MACROS
def sexpr_subst_mult_string(sexpr_or_str: SExprOrStr, strs_to_replace: Sequence[str], replacements: Sequence[SExprOrStr]) -> SExprOrStr:
    assert len(strs_to_replace) == len(replacements), f"{strs_to_replace}\n{replacements}"
    if isinstance(sexpr_or_str, str):
        return mult_replace(sexpr_or_str, strs_to_replace, replacements)
    else:
        return SExpr(lst=list(
            map(lambda child: sexpr_subst_mult_string(child, strs_to_replace, replacements), sexpr_or_str.lst)),
                     line=sexpr_or_str.line, col=sexpr_or_str.col, symb=sexpr_or_str.symb)

# inefficient
def sexpr_rewrite(sexpr:SExpr,
                  should_rewrite: Callable[[SExpr],bool],
                  rewrite:Callable[[SExpr],List[SExpr]]):
    changed = True
    while changed:
        changed = False
        for i in range(len(sexpr.lst)-1,0,-1):
            arg = sexpr.lst[i]
            if isinstance(arg,SExpr) and should_rewrite(arg):
                changed = True
                del sexpr.lst[i]
                for newarg in reversed(rewrite(arg)):
                    sexpr.lst.insert(i,newarg)
    for arg in sexpr:
        if isinstance(arg,SExpr):
            sexpr_rewrite(arg,should_rewrite,rewrite)
