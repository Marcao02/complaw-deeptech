from typing import Optional, Dict

# By age 35, you should have written an ad-hoc, informally-specified, bug-ridden, slow implementation of half of Common Lisp.
# https://en.wikipedia.org/wiki/Greenspun%27s_tenth_rule
# https://twitter.com/NeckbeardHacker/status/999087537883942912


from src.independent.util import chcaststr
from src.independent.util_for_sequences import is_singleton_string_list
from src.independent.SExpr import *

ALLOW_PRIMED_NAMES = True
FORBID_LINEBREAKS = False

class SExprBuilder:
    def __init__(self) -> None:
        # stack of growing S-Expressions
        self.stack: List[SExpr] = [SExpr([], 1, 1)]
        self.expressions_to_insert : Dict[str,SExpr] = dict()

    def openParenSeq(self,symb,line:int,col:int):
        self.stack.append(SExpr([], line, col, symb))

    def appendTokenInCurScope(self,token): 
        self.curScope.append(token)

    def closeParenSeq(self,closesymb,line,col):
        temp = self.stack.pop()
        assert temp is not None and grouper_map[temp.symb] == closesymb, ("Found " + closesymb + " while expecting " + grouper_map[temp.symb] + "\n" +
                                                                          "Left symbol line {} col {}\n".format(temp.line,temp.col) +
                                                                          "Right symbol line {} col {}".format(line,col) )
        assert len(self.stack) > 0, ("Found " + closesymb + " while expecting " + grouper_map[temp.symb] + " but self.stack is empty." + "\n" +
                                     "Left symbol line {} col {}\n".format(temp.line,temp.col) +
                                     "Right symbol line {} col {}".format(line,col) )
        self.curScope.append(temp)

    @property
    def curScope(self) -> SExpr:
        return self.stack[-1]

    def __repr__(self):
        return '*' + repr(self.stack) + '*'


def parse(string:str, debug=False, strip_comments=True, custom_splitter:Optional[Callable[[str,int],bool]] = None) -> SExpr:
    """
    >>> parse("(+ 5 (+ 3 5))")
    [['+', '5', ['+', '3', '5']]]
    >>> parse("+ 5 (+ 3 5)")
    ['+', '5', ['+', '3', '5']]
    >>> parse("(f a)")
    [['f', 'a']]
    >>> parse("f a")
    ['f', 'a']
    >>> parse("'f () is a string' a")
    [['STRLIT', 'f () is a string'], 'a']
    >>> parse('"one string" \\'another string\\'')
    [['STRLIT', 'one string'], ['STRLIT', 'another string']]
    >>> parse('"quote ` in string"')
    [['STRLIT', 'quote ` in string']]
    """
    builder = SExprBuilder() 
    word = '' 
    
    in_str_lit = False
    in_comment = False
    skip_next_char = False
    str_lit_quote = None
    next_str_lit_is_file_path = False
    
    i,line,col = 0,1,1

    def maybeAppendToken():
        nonlocal word, builder, next_str_lit_is_file_path

        if word:
            if word == PASTE_DIRECTIVE_TOKEN:
                next_str_lit_is_file_path = True
            builder.appendTokenInCurScope(word)
            word = ''

    for i in range(len(string)):
        char = string[i]

        assert not in_comment or not in_str_lit

        if not skip_next_char:
            if in_comment:
                if char == '\n':
                    in_comment = False
                    if not strip_comments:
                        maybeAppendToken()
                        builder.closeParenSeq(char, line, col)
                else:
                    if not strip_comments:
                        word += char

            elif (char in left_groupers) and not in_str_lit:
                maybeAppendToken()
                builder.openParenSeq(char, line, col)

            elif (char in right_groupers) and not in_str_lit:
                maybeAppendToken()
                builder.closeParenSeq(char,line,col)

            elif char == LINE_COMMENT_START_CHAR and not in_str_lit and not in_comment:
                in_comment = True
                if not strip_comments:
                    builder.openParenSeq(char,line,col)
                    builder.appendTokenInCurScope(COMMENT_LITERAL_MARKER)

            elif char in (' ', '\n', '\t') and not in_str_lit:
                maybeAppendToken()

            elif ALLOW_PRIMED_NAMES and (not in_str_lit) and char == "'" and i > 0 and string[i-1].isalnum():
                word += char
                maybeAppendToken()

            elif char in quotelike and ((not in_str_lit) or (str_lit_quote == char)):
                if in_str_lit:
                    in_str_lit = False
                    str_lit_quote = None
                    builder.appendTokenInCurScope(word)
                    if next_str_lit_is_file_path:
                        next_str_lit_is_file_path = False
                        external = parse_file(word, debug, strip_comments, custom_splitter)
                        builder.expressions_to_insert[word] = external
                    word = ''
                    builder.closeParenSeq(char,line,col)
                else:
                    in_str_lit = True
                    str_lit_quote = char
                    builder.openParenSeq(char, line, col)
                    builder.appendTokenInCurScope(STRING_LITERAL_MARKER)

            elif i < len(string)-1 and (char + string[i+1]) in double_splits_word_only:
                maybeAppendToken()
                word = char + string[i+1]
                skip_next_char = True
                maybeAppendToken()

            elif FORBID_LINEBREAKS and in_str_lit and char == '\n':
                assert False, "\nNo linebreaks in strings, just to prevent hard-to-diagnose syntax errors. See line {} column {}".format(line,col)

            elif in_str_lit:
                word += char

            elif char in splits_word_only:
                maybeAppendToken()
                word = char
                maybeAppendToken()

            elif custom_splitter and custom_splitter(string,i):
                maybeAppendToken()
                word = char
                maybeAppendToken()

            else:
                word += char
        else:
            skip_next_char = False

        if char == '\n':
            line += 1
            col = 1  
        else:
            col += 1        

        if debug:
            print(char, repr(builder.stack))

    maybeAppendToken()        
    
    if debug:
        print(char, repr(builder.stack))
        print("stack size: ", len(builder.stack))

    assert len(builder.stack) == 1, "Unbalanced parentheses...\n\n" + str(builder.stack[0])

    if len(builder.expressions_to_insert) > 0:
        newScope = builder.curScope.newHere([])
        for x in builder.curScope:
            if x[0] == PASTE_DIRECTIVE_TOKEN:
                filename = x[1][1]
                # discarding x
                newScope.lst.extend(builder.expressions_to_insert[filename])
            else:
                newScope.append(x)
        return newScope
    else:
        return builder.curScope

def prettySExprStr(l:Union[str, SExpr], nspaces=0) -> str:
    indent : str = " "*nspaces
    if isinstance(l,str):
        return indent + l
    elif isinstance(l,SExpr) and len(l) >= 1 and l[0] == STRING_LITERAL_MARKER:
        return indent + "`" + chcaststr(l[1]) + "`"
    else:
        lsymb = l.symb if isinstance(l,SExpr) else "("
        rsymb = grouper_map[l.symb] if isinstance(l,SExpr) else ")"
        s = ""
        line_broke = False
        for x in l:
            if isinstance(x,str):
                s += " " + x
            elif isinstance(x,list) and len(x) == 0:
                s += "()"
            elif isinstance(x,SExpr) and x.symb == '[' and is_singleton_string_list(x.lst):
                s += " [" + chcaststr(x[0]) + "]"
            elif isinstance(x,list) and len(x) >= 1 and x[0] == STRING_LITERAL_MARKER:
                s += " `" + chcaststr(x[1]) + "`"
            else:
                s += "\n" + prettySExprStr(x, nspaces + 4)
                line_broke = True
        if line_broke:
            s = indent + lsymb + s 
            s += "\n"+indent + rsymb
        else:
            s = indent + lsymb + s
            s += " " + rsymb    
        return s

def parse_file(path:str,debug=False, strip_comments=True, custom_splitter:Optional[Callable[[str,int],bool]] = None) -> SExpr:
    try:
        fil = open(path,encoding='utf8')
        parsed = parse(fil.read(), debug, strip_comments, custom_splitter)
        fil.close()
    except Exception as e:
        print(f"Problem parsing file: {path}")
        raise e
    return parsed

if __name__ == '__main__':
    # import sys
    # if 'test' in sys.argv:
    import doctest
    print("Running tests")
    doctest.testmod()
