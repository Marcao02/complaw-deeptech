from typing import NamedTuple, Tuple, Set, Dict, Union, TypeVar, Sequence, Generator, Any, cast, Optional, List, \
    Callable

# I'm implementing NoLockin for GC-collected languages.
# Our first use, the symbolic exec alg, is a pure function.

# ✓ 1 parse as s-expression (using existing code!! can replace later if get first version finished!!)
# 2 convert to Node tree form. This includes making .call nodes. (head:dotcall, ch:(obj fn arg1 arg2))
# ✓ 3a collect macros
# ✓ 3b do recursive expansion in macro bodies, bottom-up
# ✓ 3c expand macros in main program
# ✓ 4 replace ** and overloads (easy, cuz for now same name requires different # params)
# 5 generate subtyping DAG.
# 6 bidirectional typing and insert coercions. if there's more than 1 path, you just need to give enough of the coercions manually that there's only one path.
# 7 eliminte dot call syntax in favor of Namespace.fn form (even in languages with dynamic dispatch, since more efficient)
# 8 hard-coded routine for expanding into some mainstream language

# a Node is a string or a struct with fields
# head: str - can be absfn, tns, macro, (, etc
# ch: Tup[TE]
# meta: TEMeta

# inefficient but ok-for-now alg:
# depth of a macro is depth of its body.
# node with no macro applications is already expanded, and has depth 0.
# repeat till no change:
#   expand all depth-0-macro applications to depth-0 nodes.
from src.independent.SExpr import SExprOrStr
from src.independent.parse_sexpr import parse_file, prettySExprStr
from src.independent.util import chcast, chcaststr

T = TypeVar('T')
Tup = Tuple[T,...]

class NodeMeta(NamedTuple):
    grouper_symb: str
    line: int
    col: int


class Node(NamedTuple):
    ch: Tuple[Union['Node',str],...]
    meta: NodeMeta

    @property
    def head(self) -> Union['Node',str]:
        assert len(self.ch) > 0, self
        return self.ch[0]

    # def tillEnd(self,i:int) -> 'Node':
    #     return Node(self.ch[i:], self.meta)
    # def newHere(self, ch: Tuple[Union['Node',str],...]) -> 'Node':
    #     return Node(ch, self.meta)
    # def fromStartToExclusive(self,i:int) -> 'Node':
    #     return Node(self.ch[:i], self.meta)
    # def withElementDropped(self,i:int) -> 'Node':
    #     return Node(self.ch[:i] + self.ch[i + 1:], self.meta)
    # why did this cause an infinite loop?
    # def __getitem__(self, item):
    #     return self.ch.__getitem__(item)

    def __len__(self):
        return len(self.ch)

    def __iter__(self):
        return self.ch.__iter__()

    def __str__(self):
        return f"Node{str(self.ch)}"

    def __repr__(self):
        # return f"Node({repr(self.ch)},{repr(self.meta)})"
        return f"Node({repr(self.ch)})"

NodeOrStr = Union[Node,str]

def prettyNodeStr(l:NodeOrStr, nspaces=0) -> str:
    indent : str = " "*nspaces
    if isinstance(l,str):
        return indent + l
    elif l is None:
        return indent + str(l)
    else:
        # lsymb = l.meta.symb if isinstance(l,Node) else "("
        # rsymb = grouper_map[l.symb] if isinstance(l,Node) else ")"
        lsymb, rsymb = "(",")"
        s = ""
        line_broke = False
        for x in l:
            if isinstance(x,str):
                s += " " + x
            elif isinstance(x,tuple) and len(x) == 0:
                s += " ()"
            # next only good if i also checked that parent is a macro node
            # elif isinstance(x,tuple) and all(isinstance(z,str) for z in x):
            #     s += " " + prettyNodeStr(cast(Any, x))
            else:
                s += "\n" + prettyNodeStr(cast(Any,x), nspaces + 4)
                line_broke = True

        if line_broke:
            s = indent + lsymb + s 
            s += "\n"+indent + rsymb
        else:
            s = indent + lsymb + s
            s += " " + rsymb    
        return s


def mdef2name(u:Node) -> str:
    assert len(u) >= 4, "macro node has wrong form: " + str(u)
    return chcaststr(u.ch[1])
def mdef2paramnames(u:Node) -> Tup[str]:
    assert len(u) >= 4, "macro node has wrong form: " + str(u)
    return cast(Tup[str],cast(Node,u.ch[2]).ch)
def mdef2body(u:Node) -> NodeOrStr:
    assert len(u) >= 4, "macro node has wrong form: " + str(u)
    return cast(NodeOrStr,u.ch[3])
def ismdef(u:Node) -> bool:
    rv = len(u) > 0 and (u.ch[0] == 'macro' or u.ch[0] == 'alias')
    if rv:
        assert len(u) >= 4, f"macro def {u} should have length 4"
    return rv
def ismapp(u:Node,mdefs:Dict[str,Node]) -> bool:
    return len(u) > 0 and u.ch[0] in mdefs
def mapp2name(u:Node) -> str:
    assert len(u) > 0
    return chcaststr(u.ch[0])
def mapp2args(u:Node) -> Any:
    assert len(u) > 0
    # u.ch[0] is the macro names
    # does this delay a
    return (x for x in u.ch[1:])

def holds_for_all_NodeOrStr(u:NodeOrStr, prop:Callable[[NodeOrStr],bool]) -> bool:
    return prop(u) and (isinstance(u,str) or all(holds_for_all_NodeOrStr(v,prop) for v in u.ch))
def assert_for_all_NodeOrStr(u:NodeOrStr, prop:Callable[[NodeOrStr],bool], msg:Optional[Callable[[],str]] = None):
    assert prop(u), f"Node\n{u}\nviolates given property.\n" + ("" if not msg else msg())
    if not isinstance(u,str):
        for v in u.ch:
            assert isinstance(v,Node) or isinstance(v,str), f"Node:\n{u}\nhas an illegal child:\n{v}.\n"
            assert_for_all_NodeOrStr(v, prop, msg)

def convert_nodes_bottom_up(u:NodeOrStr, converter:Callable[[Node],Node]) -> NodeOrStr:
    if isinstance(u,str):
        return u
    newch = tuple(convert_nodes_bottom_up(v,converter) for v in u.ch)
    return converter(Node(newch,u.meta))

def filter_nodes(u:NodeOrStr, filter:Callable[[Node],bool]) -> Set[Node]:
    if isinstance(u,str):
        return set()
    rv = set(u) if filter(u) else set()
    for v in u.ch:
        rv.update(filter_nodes(v,filter))
    return rv

def collect_partialfn_range(u:NodeOrStr, pfn:Callable[[NodeOrStr],Optional[T]]) -> Set[T]:
    result = pfn(u)
    rv = set((result,)) if (result is not None) else set()
    if isinstance(u,str):
        return rv
    for v in u.ch:
        rv.update(collect_partialfn_range(v,pfn))
    return rv

def sexpr_to_node(se:SExprOrStr) -> NodeOrStr:
    if isinstance(se,str):
        return se
    return Node(ch=tuple(sexpr_to_node(x) for x in se.lst),
                meta=NodeMeta(se.symb,se.line,se.col))

def collect_macro_defs(top:Node) -> Dict[str,Node]:
    rv = dict()
    def helper(v:NodeOrStr):
        if isinstance(v,str) or len(v) == 0:
            return
        else:
            if ismdef(v):
                rv[mdef2name(v)] = v
                # helper(mdef2body(v))
            else:
                for x in v.ch:
                    helper(x)
    helper(top)
    return rv

# if news[i] is a str, it will be substituted for olds[i] even within strings that contains olds[i] as a string substring
# if news[i] is a Node, it will be substituted for olds[i] only when olds[i] is the whole string.
def crude_subst(u: NodeOrStr, olds: Sequence[str], news: Sequence[NodeOrStr]) -> Tuple[NodeOrStr,bool]:

    def replace(s:str) -> Tuple[NodeOrStr,bool]:
        # print(f"replace({s})")
        rv = s
        for i in range(len(olds)):
            assert isinstance(olds[i],str), str(olds) + "   " + str(type(olds)) + "   " + str(type(olds[i]))
            if isinstance(news[i],str):
                rv = rv.replace(olds[i], chcaststr(news[i]))
            # if a string is exactly the parameter name, only in that case can we substitute in a Node
            elif s == olds[i]:
                return news[i], True

        return rv, rv is not s

    assert len(olds) == len(news), f"len({olds}) ≠ len({news})"
    if isinstance(u, str):
        return replace(u)
    else:
        crude_subst_rvs = tuple(crude_subst(child, olds, news) for child in u.ch)
        changed = not all(not rv[1] for rv in crude_subst_rvs)
        if not changed:
            return u,False
        else:
            return Node(ch= tuple(rv[0] for rv in crude_subst_rvs),
                    meta= u.meta), True

def has_depth0(u:NodeOrStr, mdefs:Dict[str,Node]) -> bool:
    if isinstance(u,str):
        return True
    if ismapp(u,mdefs):
        return False
    if ismdef(u):
        return has_depth0(mdef2body(u), mdefs)
    return all(has_depth0(x, mdefs) for x in u.ch)


# Inefficient! Should've computed the dependency DAG. Don't judge me.
def expand_macro_defs(mdefs:Dict[str,Node]):
    # we'll be modifying mdefs
    depth0macronames : Set[str] = set()
    changed = False

    def with_depth0_macro_apps_expanded(v:NodeOrStr) -> NodeOrStr:
        nonlocal changed

        def h(u):
            nonlocal changed
            if isinstance(u,str):
                return u

            if ismapp(u,mdefs):
                mname = mapp2name(u)
                mappargsmod = tuple(h(arg) for arg in mapp2args(u))
                if mname in depth0macronames:
                    args_are_depth_0 = all(has_depth0(x,mdefs) for x in mappargsmod)
                    mdef = mdefs[mname]
                    mbody = mdef2body(mdef)
                    mparams = mdef2paramnames(mdef)
                    if args_are_depth_0:
                        changed = True
                        crude_subst_rv = crude_subst(mbody, mparams, mappargsmod)
                        assert crude_subst_rv[1]
                        return crude_subst_rv[0]
                    else:
                        return Node(cast(Tup[NodeOrStr],(mdef[0],mname,mparams,mbody)),u.meta)
                else:
                    return Node(tuple(h(x) for x in mappargsmod),
                                u.meta)
            if ismdef(u):
                mname = mdef2name(u)
                if mname not in depth0macronames and has_depth0(mdef2body(u),mdefs):
                    changed = True
                    depth0macronames.add(mname)
                else:
                    # can this be skipped?
                    mdefs[mname] = Node(
                        tuple(h(x) for x in u.ch),
                        u.meta)
                    return mdefs[mname]

            return Node(tuple(h(x) for x in u.ch),
                        u.meta)
        return h(v)

    changed = True
    iters = 0
    mnames = mdefs.keys()
    while changed:
        iters += 1
        changed = False
        for mname in mnames:
            with_depth0_macro_apps_expanded(mdefs[mname])
    print(f"expand_macro_defs completed in {iters} iterations.")


def eliminate_depth0_macros(top:Node, mdefs:Dict[str,Node]):
    def h(u):
        if isinstance(u,str):
            return u
        # elimination happens bottom-up
        ch = tuple(h(v) for v in u.ch)
        if ismapp(u, mdefs):
            mname = mapp2name(u)
            mdef = mdefs[mname]
            mbody = mdef2body(mdef)
            if mdef.ch[0] == 'macro':
                margs = mdef2paramnames(mdef)
                assert len(margs) == len(ch) - 1, f"\n{len(ch)} {len(margs)}\nu: {u}\nmargs: {margs}\nch: {ch}"
                rv = crude_subst(mbody, margs, ch[1:])[0]
            elif mdef.ch[0] == 'alias':
                rv = mbody
            else:
                raise Exception(f"\n{u}\n{mdef.ch[0]}\n{mdef}")

        else:
            rv = Node(ch, u.meta)
        return rv

    # mdef2 = {mname: h(mdef2body(mdef)) for mname,mdef in mdefs.items()}
    # for mname in mdef2:
    #     mdefs[mname] = mdef2[mname]
    return h(top)

def eliminate_macro_defs(top:Node) -> Node:

    return Node(tuple(filter(lambda x: isinstance(x,str) or (x.ch[0] != 'macro' and x.ch[0] != 'alias'),
                             top.ch)),
                top.meta)


def eliminate_self_and_overloads_from_macroless_node(top:Node) -> Node:

    # overloads_by_tns : Dict[str,]

    def h(u:NodeOrStr):
        if isinstance(u,str) or len(u) == 0:
            return u
        if u.head == 'tns':
            if len(u) == 2:
                return u
            cur_tns_name = u.ch[1]
            fn_names : Set[str] = set()
            doubles : Set[str] = set()
            # print(cur_tns_name, str(u))
            new_uch : List[NodeOrStr] = ['tns',cur_tns_name]
            change_made = False
            for v in u.ch[2:]:
                assert isinstance(v,Node), f"Should be only nodes in (tns node).ch[2:] == {u.ch[2:]} but found {v}"
                assert len(v) > 0, "There should be no empty Node immediately inside the third element of a Node with head 'tns'"
                if v.head != 'absfn':
                    continue

                name = chcaststr(v.ch[1])
                if name in fn_names:
                    doubles.add(name)
                else:
                    fn_names.add(name)

                replace_self_result = crude_subst(v, ("**",), (cur_tns_name,))
                if replace_self_result[1]:
                    change_made = True
                    new_uch.append(replace_self_result[0])
                else:
                    new_uch.append(v)

            new_uch2: List[NodeOrStr] = ['tns',cur_tns_name]
            for v in new_uch[2:]:
                assert isinstance(v, Node), f"Should be only nodes in (tns node).ch[2:] == {u.ch[2:]} but found {v}"
                assert len(v) > 0, "There should be no empty Node immediately inside the third element of a Node with head 'tns'"
                if v.head != 'absfn':
                    continue
                name = chcaststr(v.ch[1])
                if name in doubles:
                    change_made = True
                    arity = len(v.ch[2])
                    # give it a new name distinct from its siblings
                    newparts = ('absfn', name + str(arity))
                    if len(v.ch) == 5:
                        # has parameters
                        params_node = chcast(Node,v.ch[2])
                        params = params_node.ch
                        newparams = tuple(cur_tns_name if typ == '**' else typ for typ in params)
                        new_vch = cast(Tuple[NodeOrStr], newparts + (Node(newparams,params_node.meta), '->', v.ch[4]))
                    else:
                        assert len(v.ch) == 4
                        new_vch = newparts + ('->', v.ch[3])
                    new_uch2.append(Node(new_vch,v.meta))
                else:
                    new_uch2.append(v)
            return Node(tuple(new_uch2), u.meta) if change_made else u
        return Node(tuple(h(child) for child in u.ch), u.meta)
    return h(top)

def check_dot_calls(top:Node):
    def dot_correctness(u:NodeOrStr) -> bool:
        if isinstance(u,str):
            return True
        if '.' not in u.ch:
            return True
        # last occurrence must be at position 2
        # and next child must exist and be a string
        return (len(u.ch) >= 3) and (u.ch[1] == '.') and isinstance(u.ch[2],str) and (tuple(reversed(u.ch)).index('.') == len(u.ch) - 2)
        # and next child must exist and be a string
    assert_for_all_NodeOrStr(top, dot_correctness, lambda: "If '.' occurs in a tuple, then its only occurrence must be at position 2 and the element following it must be a string")

def printTop(top:Node):
    for dec in top.ch:
        print(prettyNodeStr(dec))

def dot_split_condition(s:str, i:int):
    return (0 < i < len(s)-1) and (
        s[i] == '.' and (not s[i-1].isdigit() or not s[i+1].isdigit())
    )

def convert_dot_calls(u:Node, tns_names:Set[str]):
    def converter(v:Node) -> Node:
        if len(v.ch) >= 3 and v.ch[1] == '.':
            if v.ch[0] in tns_names:
                # (type . f y z) becomes (type.f x y z)
                return Node((v.ch[0] + "." + v.ch[2],) + v.ch[3:],
                     v.meta)
            else:
                # (x . f y z) becomes (.f x y z)
                return Node(("." + v.ch[2], v.ch[0]) + v.ch[3:],
                            v.meta)
        return v

    return convert_nodes_bottom_up(u,converter)


parsed = parse_file("SymbExecTimelessLSM.nl", False, True, dot_split_condition)
# print(prettySExprStr(parsed))
topnode = chcast(Node,sexpr_to_node(parsed))
# print(repr(topnode))
mdefs = collect_macro_defs(topnode)

# for k,v in mdefs.items():
#     print(f"{prettyNodeStr(v)}\n")

expand_macro_defs(mdefs)
# printTop(topnode)

# for k,v in mdefs.items():
#     print(f"{prettyNodeStr(v)}\n")

topnode = eliminate_macro_defs(topnode)
topnode = eliminate_depth0_macros(topnode, mdefs)


topnode = eliminate_self_and_overloads_from_macroless_node(topnode)

def maybe_to_tns_name(v:NodeOrStr) -> Optional[str]:
    if isinstance(v,str): return None
    if len(v.ch) > 0 and v.ch[0] == 'tns': return v.ch[1]
    return None


tns_names = collect_partialfn_range(topnode, maybe_to_tns_name)
print("tns_names:", tns_names)

check_dot_calls(topnode)
topnode = convert_dot_calls(topnode, tns_names)
printTop(topnode)
print("""We can't add the arity numbers to all functions yet, just due to the case where some name N is used
in two TNSs, but it's overloaded in only one of them. So we'll  """)



# printTop(topnode)
