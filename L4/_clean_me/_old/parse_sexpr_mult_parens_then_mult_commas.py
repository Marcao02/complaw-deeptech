#! /usr/bin/env python3
from typing import Union
from l4fnc_util import singleton_string_list

left_groupers = set(['(','{','[','<','‹','❪'])
right_groupers = set([')','}',']','>','›','❫'])
grouper_map = {'(':')', '{':'}', '[':']', '<':'>', '‹':'›', '❪':'❫'}
commalike = set([',',';'])
quotelike = set(["'",'"'])

class TaggedList:
    def __init__(self,symb=None,lst=None):
        assert not symb or isinstance(symb,str)
        assert not lst or isinstance(lst,list)
        self.lst = lst if lst else []
        self.symb = symb

    def append(self,x):
        self.lst.append(x)

    def pop(self):
        return self.lst.pop()

    def top(self):
        return self.lst[-1]

    def __repr__(self):
        return repr(self.lst)

class Com(TaggedList):
    def __repr__(self):
        return 'Com('+repr(self.lst)+')'

class Par(TaggedList):
    def __repr__(self):
        return 'Par('+repr(self.lst)+')'

class Toplev(TaggedList):
    def __init__(self,lst=None):
        super().__init__(None,lst)

    def __repr__(self):
        return 'Toplev('+repr(self.lst)+')'

class StrLit():
    def __init__(self,s:str):
        self.string = s

    def __repr__(self):
        return "'" + self.string + "'"


def to_sexpr_tuple(t:Union[TaggedList,str,StrLit]):
    if isinstance(t,str):
        return t
    elif isinstance(t,StrLit):
        return ("StrLit",t.string)
    # elif isinstance(t,Toplev):
    #     return (to_sexpr_tuple(x) for x in t.lst)
    elif t is None:
        return ()
    else:
        assert isinstance(t,TaggedList), t
        return tuple(to_sexpr_tuple(x) for x in t.lst)


class SExprBuilder:
    def __init__(self):
        # stack of growing S-Expressions
        self.stack = [Toplev([])]

    def openParenSeq(self,symb):
        x = Par(symb)
        self.stack.append(x)

    def openCommalikeDelimSeq(self,symb):
        assert symb in commalike
        x = Com(symb)
        self.stack.append(x)

    def appendTokenInCurScope(self,token):
        self.curScope.append(token)

    def closeParenSeq(self,symb):
        temp = self.stack.pop()
        assert temp is not None and isinstance(temp,Par) and grouper_map[temp.symb] == symb, ("Found " + symb + " while expecting " + grouper_map[temp.symb])
        self.curScope.append(temp)

    def closeCommalikeDelimSeq(self,symb):
        temp = self.stack.pop()
        assert temp is not None and isinstance(temp,Com) and temp.symb == symb, (symb + "!=" + temp.symb)
        self.curScope.append(temp)

    def closeTopSeq(self):
        temp = self.stack.pop()
        assert isinstance(self.curScope,Toplev)
        assert temp is not None and isinstance(temp,Toplev), temp
        self.curScope.append(temp)

    def wrapCurSeqInCommaLike(self,symb):
        scope = self.curScope
        scope.lst = [Com(symb,scope.lst)]
        # self.stack[-1] = Com(symb,self.curScope.lst)

    def wrapStackInToplevel(self):
        seq = Toplev([x for x in self.stack])
        self.stack[-1] = Toplev([x for x in self.stack])

    @property
    def curScope(self):
        return self.stack[-1]

    def __repr__(self):
        return '*' + repr(self.stack) + '*'


parse_sexp_mutline_doctests = [
    """
         foo (x : ℕ, y : str) : stuff {
            (body 1)
            (body 2 (3 4 5) 6)
            ("a string ()")
        }
    """
]

def parse_parens(string:'String', debug=False):
    """
    >>> parse_parens("(+ 5 (+ 3 5))")
    Toplev([Par(['+', '5', Par(['+', '3', '5'])])])
    >>> parse_parens("(f a)")
    Toplev([Par(['f', 'a'])])
    >>> parse_parens("f a")
    Toplev(['f', 'a'])
    >>> parse_parens("f,a")
    Toplev(['f', ',', 'a'])
    >>> parse_parens("[f,(a;b)]")
    Toplev([Par(['f', ',', Par(['a', ';', 'b'])])])
    >>> parse_parens("(f,a)")
    Toplev([Par(['f', ',', 'a'])])
    """
    builder = SExprBuilder()
    word = ''

    string = '(' + string + ')'

    str_lit_opened_at = None
    in_str_lit = False
    str_lit_quote = None

    i,line,col = 0,1,1

    def maybeAppendToken():
        nonlocal word, builder
        if word:
            builder.appendTokenInCurScope(word)
            word = ''


    for i in range(len(string)):
        char = string[i]

        if (char in left_groupers) and not in_str_lit:
            maybeAppendToken()
            builder.openParenSeq(char)

        elif (char in right_groupers) and not in_str_lit:
            maybeAppendToken()
            try:
                builder.closeParenSeq(char)
            except Exception as e:
                print("\nFailed trying to close a {} at line {}, col {}".format(char,line,col))
                print(e,"\n")
                raise e

        elif (char in commalike) and not in_str_lit:
            maybeAppendToken()
            word = char
            maybeAppendToken()

        elif char in (' ', '\n', '\t') and not in_str_lit:
            maybeAppendToken()

        elif char in quotelike and ((not in_str_lit) or (str_lit_quote == char)):
            if in_str_lit:
                in_str_lit = False
                str_lit_quote = None
                str_lit_opened_at = None
                builder.appendTokenInCurScope(StrLit(word))
                word = ''
            else:
                in_str_lit = True
                str_lit_quote = char
                str_lit_opened_at = (i,line,col)

        elif in_str_lit and char == '\n':
            assert False, "\nNo linebreaks in strings, just to prevent hard to diagnose syntax errors. See line {} column {}".format(line,col)

        else:
            word += char

        if char == '\n' and not in_str_lit:
            line += 1
            col = 1
        else:
            col += 1

        if debug:
            print(char, repr(builder.stack))

    maybeAppendToken()

    if debug:
        print(char, repr(builder.stack))
    # if len(builder.stack) > 1:
    #     builder.wrapStackInToplevel()
    # if debug:
    #     print(char, repr(builder.stack))
    # return builder.curScope

    print("stack size: ", len(builder.stack))

    # print(builder.curScope)

    return builder.curScope



def splitList(lst:list) -> list:
    """
    lst's elements are the results of calls to parse_commalike already
    """

    found = None
    # fixme: inefficient
    for c in commalike:
        if c in lst:
            assert found is None, "Can't have multiple comma-like chars in one list"
            found = c

    if not found:
        return lst

    lists = []
    nextlst = []
    for i in range(len(lst)):
        if lst[i] == found:
            lists.append(Com(found,nextlst))
            nextlst = []
        else:
            nextlst.append(lst[i])

    if len(nextlst) > 0:
        lists.append(Com(found,nextlst))

    return lists
    # lst.clear()
    # for x in lists:
    #     lst.append(x)


def parse_commalike(expr):
    if isinstance(expr,str):
        return expr

    elif isinstance(expr, StrLit):
        return expr.string

    elif isinstance(expr,Par) or isinstance(expr,Toplev):
        rec_results = list(map(parse_commalike, expr.lst))
        # print(rec_results)
        # for  in expr.lst:
        #     parse_commalike(x)
        return splitList(rec_results)

    else:
        assert False, str(isinstance(expr,str)) + expr

    # return expr

def parse_all(s:str):
    """
    >>> parse_all("f,a")
    Toplev([Com(['f']), Com(['a'])])
    >>> parse_all("(f,a)")
    Toplev([Par([Com(['f']), Com(['a'])])])
    >>> parse_all("[f,(a;b)]")
    Toplev([Par([Com(['f']), Com([Par([Com(['a']), Com(['b'])])])])])
    """
    x = parse_parens(s)
    print("\nResult of parse_parens: ")
    print(x)

    print("\nComputing parse_commalike: ")
    rv = parse_commalike(x)
    print("\nResult of parse_commalike: ")
    print(rv)
    print(pretty_generic(rv))
    return

def pretty_generic(expr:Union[TaggedList,str],nspaces=0):
    indent = nspaces*" "
    if isinstance(expr,str):
        return indent + expr
    s = ""

    if isinstance(expr, Com):
        s = pretty_generic(Par('(',expr.lst), nspaces)

    elif isinstance(expr, Par):
        if all([isinstance(x,Com) and singleton_string_list(x.lst) for x in expr.lst]):
            s = indent + expr.symb + ' ' + ', '.join([pretty_generic(x.lst[0],0) for x in expr.lst]) + ' ' + grouper_map[expr.symb]
        elif all([isinstance(x,str) for x in expr.lst]):
            s = indent + expr.symb + ' ' + ' '.join([pretty_generic(x,0) for x in expr.lst]) + ' ' + grouper_map[expr.symb]
        elif all([isinstance(x,Com) for x in expr.lst]):
            s = indent + expr.symb + '\n'
            # s += expr.lst[0].symb + ",\n".join([pretty_generic(x,nspaces+2) for x in expr.lst])
            s += (expr.lst[0].symb + "\n").join([pretty_generic(x,nspaces+2) for x in expr.lst])
            s += '\n' + indent + grouper_map[expr.symb]

        else:
            s = indent + expr.symb + '\n'
            s += "\n".join([pretty_generic(x,nspaces+2) for x in expr.lst])
            s += '\n' + indent + grouper_map[expr.symb]

    elif isinstance(expr, Toplev):
        assert nspaces == 0

        s = "Top:\n" + "\n".join([pretty_generic(x,2) for x in expr.lst])

    elif isinstance(expr, StrLit):
        s = indent + "\"" + expr.string + "\""
    elif expr is None:
        s = indent + "pretty prninter found None?"
    else:
        assert False, expr

    return s

def pretty_nolockin(expr:Union[TaggedList,str],nspaces=0):
    if isinstance(expr,str):
        return expr

    indent = nspaces*" "
    if isinstance(expr,TaggedList):
        lst = expr.lst
        head = lst[0]
        if head == "type&namespace":
            return indent + " ".join((pretty_nolockin(x) for x in lst[:-1])) + ' ' + pretty_nolockin(lst[-1])

        if head == "absfn":
            print("HERE?")
            s = indent + " ".join((pretty_nolockin(x) for x in lst))
            return

        if isinstance(head,Com) and head.symb == ";":
            return ";\n".join([pretty_nolockin(x) for x in lst]) + ";"
            # lst2 = head.lst
            # head2 = lst[0]

    return pretty_generic(expr)

FILES = (
    '/Users/dustin/NoLockin/NoLockin Datatypes/Collections/Unordered/Set/SetMonotonic.nlo',
    '/Users/dustin/NoLockin/NoLockin Datatypes/Collections/Unordered/Set/Set.nlo',
    '/Users/dustin/Sites/legalese/complaw-deeptech/flat-nonconcurrent-language/printer.L4fnc'
)

if __name__ == '__main__':
    import sys
    if 'test' in sys.argv:
        import doctest
        doctest.testmod()

    if 'file' in sys.argv:
        for path in FILES:
            print("\nLooking at file " + path + ":\n")
            fil = open(path,'r')
            parsed = parse_all(fil.read())
            print( parsed, "\n")
            print( to_sexpr_tuple(parsed), "\n" )
            print( pretty_generic(parsed) )


    # parse_parens("(f,a)", debug=True)
    # print(parse_parens("f a b , c"))
    # print(pretty_generic(parse_parens("f a b , c")))
    # print(pretty_generic(parse_parens("(f a b , d, c)")))
    # print(pretty_generic(parse_parens("((f a b) (x), c)")))

    # # print(parse_parens("f,a"))

    # print( pretty_generic(
    #     parse_parens( """
    #      foo (x : ℕ, y : str) : stuff {
    #         (body 1)
    #         (body 2 (3 4 5) 6)
    #         ("a string ()")
    #     }
    #     """)  )
    # )

    # print("TODO maybe (WORK OUT ON PAPER FIRST): remember in TaggedList the start and stop line nums. Then should be able to pretty_generic print almost exactly what was parsed.")
    # print(
    #     pretty_generic(parse_parens( """
    #      foo (x : ℕ, y : str) : stuff {
    #         (body 1)
    #         (body 2 (3 4 5) 6)
    #         ("a string ()")
    #     }
    #     """))
    # )





    # print(
    #     parse_parens( """
    #          foo (x : ℕ, y : str) : stuff
    #             (body 1)
    #             (body 2 (3 4 5) 6)
    #             ("a string ()")
    #         """)
    # )

    # print( pretty_generic(
    #     parse_parens( """
    #          foo (x : ℕ, y : str) : stuff {
    #             (body 1)
    #             (body 2 (3 4 5) 6)
    #             ("a string ()")
    #         }
    #         """)
    #         )
    # )


