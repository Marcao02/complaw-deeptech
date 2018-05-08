from distutils.file_util import write_file
from typing import Any, Dict, Optional

from src.constants_and_defined_types import ContractParamId, FULFILLED_SITUATION_LABEL
from src.independent.util_for_dicts import partitionBy
from src.independent.util_for_io import writeFile
from src.model.Action import Action
from src.model.ActionRule import ActionRule, PartyNextActionRule, EnvNextActionRule
from src.model.Block import Block
from src.model.BoundVar import ActionBoundActionParam, StateVar, ContractParam, LocalVar
from src.model.ContractParamDec import ContractParamDec
from src.model.L4Contract import L4Contract

import dominate
from dominate.tags import *

from src.model.Situation import Situation
from src.model.Sort import Sort
from src.model.Statement import Statement, StateVarAssign, LocalVarDec
from src.model.Term import Term, FnApp
from test.active_examples import EXAMPLES_HTML_ROOT

sort_descriptions = {
    "$": "USD amount",
    "Pos$": "positive USD amount",

    "Fraction(0,1]": "fraction in the range (0,1]",
    "ShareCnt": "number of shares",
    "PosShareCnt": "positive number of shares",

    "SharePrice": "price in USD/share",

    "TimeDelta": "number of days",
}

CSS = """
body {
    margin-bottom : 1000px;
}
.nomarkers {
    list-style-type: none;
}
.role {
    
}
.actionword {
    
}
.situationword {

}
.symbintro {
    color: red;
}
.is_assignment {
    font-weight: bold;
}
.boiler {
    font-weight: bold;
}
"""

def boil(s:Any):
    return span(s,cls="boiler")

def actiontitle(act:Action) -> Any:
    return h3(span(act.allowed_subjects[0],cls="role"),
              span(" action ",cls="actionword"),
              act.nlg,
              id=act.action_id)

def situationtitle(sit:Situation) -> Any:
    return h3(span("Scenario: ",cls="situationword"),
              sit.nlg,
              id=sit.situation_id)

def indented(x:Optional[Any] = None) -> Any:
    if x:
        return ul(x,cls="nomarkers")
    else:
        return ul(cls="nomarkers")

def one_indented(x:Any) -> Any:
    return indented(li(x))

def gen_english(prog:L4Contract, outpath:str) -> str:
    def sitid2link(sitid:str) -> Any:
        sit = prog.situation(sitid)
        return a(sit.nlg, href=f"#{sitid}")
    def actid2link(actid:str) -> Any:
        act = prog.action(actid)
        return a(act.nlg, href=f"#{actid}")
    def id2link(nameid:str, ctx:Optional[str] = None) -> Any:
        if ctx:
            return a(maybeNL(nameid), href=f"#{ctx}.{nameid}")
        else:
            return a(maybeNL(nameid), href=f"#{nameid}")

    def intro(nameid: str) -> Any:
        return span(maybeNL(nameid), id=nameid, cls="symbintro")
    def actionparamIntro(pname: str, sort: Sort, actid:str):
        return li(intro(pname), ", which is a " + sortHtml(sort), id=f"{actid}.{pname}")

    def maybeNL(s:str) -> str:
        return prog.nlg_names[s] if s in prog.nlg_names else s

    def contract_params_section() -> Any:
        params = prog.contract_params
        rv = div()
        rv.add(div("The parameters to the contract are:"))
        cpul = rv.add(indented())
        for para,dec in params.items():
            cpul.add( li( intro(para), ", which is a ", f"{sortHtml(dec.sort)}.") )
        return rv

    def state_vars_section() -> Any:
        svars = prog.state_var_decs
        rv = div()
        rv.add(div("The contract tracks the following variables:"))
        cpul = rv.add(indented())
        for svar,dec in svars.items():
            if dec.initval is not None:
                cpul.add(li(intro(svar), ", which is a ", f"{sortHtml(dec.sort)} with initial value ", termHtml(dec.initval), "."))
            else:
                cpul.add( li( intro(svar), ", which is a ", f"{sortHtml(dec.sort)}.") )
        return rv

    def sortHtml(sort:Sort) -> Any:
        return sort_descriptions[str(sort)]

    def situationHtml(sit:Situation, is_anon=False) -> Any:
        # show two rules with the same action-enabled guard together
        rules_by_enabled_guard = partitionBy(lambda x: x.entrance_enabled_guard, sit.action_rules())
        sitsec = li()
        sitsec.add(situationtitle(sit))
        rules = sitsec.add(indented())
        for guardkey,rulelist in rules_by_enabled_guard.items():
            if guardkey != "None":
                rulegroup = rules.add(li(span("if ", termHtml(rulelist[0].entrance_enabled_guard)))).add(indented())
                for rule in rulelist:
                    rulegroup.add(ruleHtml(rule))
            else:
                # no action-enabled guard
                for rule in rulelist:
                    rules.add(ruleHtml(rule))

        return sitsec

    def actionHtml(act: Action) -> Any:
        # actsec = indented()
        # container.add(actsec)
        rv = li(actiontitle(act))
        actcontents = rv.add(indented())
        if len(act.param_names) > 0:
            actcontents.add(div(act.allowed_subjects[0] + " must provide:"))
            actcontents.add(actionparamsHtml(act.param_sorts_by_name, act.action_id))
            actcontents.add(br())
        if act.state_transform:
            actcontents.add(div("Define:"))
            statetrans = actcontents.add(indented())
            statetrans.add(blockHtml(act.state_transform.statements))
            actcontents.add(br())

        if act.following_anon_situation:
            actcontents.add(situationHtml(act.following_anon_situation, True))
        elif act.dest_situation_id == FULFILLED_SITUATION_LABEL:
            actcontents.add(div("Contract fulfilled."))
        else:
            actcontents.add(div("Go to ", sitid2link(act.dest_situation_id)))
        return rv

    def actionparamsHtml(d:Dict[str, Sort], actid:str) -> Any:
        rv = indented()
        for pname,sort in d.items():
            rv.add(actionparamIntro(pname,sort,actid))
            # rv.add(li(pname + ", which is a " + sortHtml(sort)))
        return rv

    def blockHtml(block:Block) -> Any:
        rv = div()
        for statement in block:
            rv.add(statementHtml(statement))
        return rv

    def statementHtml(statement:Statement) -> Any:
        if isinstance(statement,StateVarAssign):
            # return div(statement.varname, span("  is  ",cls="is_assignment"), one_indented(termHtml(statement.value_expr)))
            return div(id2link(statement.varname), span(" is:"), one_indented(termHtml(statement.value_expr)))
        elif isinstance(statement,LocalVarDec):
            return div("Temporarily, a ", sortHtml(statement.sort), ", ", intro(statement.varname), ", by  ", one_indented(termHtml(statement.value_expr)))
        return div(str(statement))

    def timedeltaLitHtml(s:str) -> str:
        if s.endswith("d"):
            if int(s[:-1]) == 1:
                return s[:-1] + " day"
            else:
                return s[:-1] + " days"
        elif s.endswith("w"):
            if int(s[:-1]) == 1:
                return s[:-1] + " week"
            else:
                return s[:-1] + " weeks"

        raise NotImplementedError(s)

    def ruleHtml(rule:ActionRule):
            
        if isinstance(rule, PartyNextActionRule):
            x = li(
                span(rule.role_ids[0]),
                span(f" {rule.deontic_keyword} "),
                actid2link(rule.action_id)
            )
        elif isinstance(rule, EnvNextActionRule):
            if "Breach" in rule.action_id:
                x = li(
                    span(rule.action_id[7:], " breaches the contract.")
                )
            else:
                x = li(
                    span("Go to "),
                    actid2link(rule.action_id)
                )
        else:
            raise NotImplementedError

        if rule.where_clause:
            x = li(span("if ", termHtml(rule.where_clause)),
                   indented(x))
        if rule.time_constraint:
            if rule.time_constraint.src_expr:
                y = rule.time_constraint.src_expr
                if y[0] in {"before_split", "within_split", "at_split", "after_split", "at_or_after_split"}:
                    if y[0] == "within_split":
                        x = li(span("Within ", timedeltaLitHtml(y[1]), " of the last event:"),
                               indented(x))
                    elif y[0] == "at_split":
                        x = li(span("At ", timedeltaLitHtml(y[1]), " since the last event:"),
                               indented(x))
                    elif y[0] == "before_split":
                        x = li(span("Before ", timedeltaLitHtml(y[1]), " since the last event:"),
                               indented(x))
                    elif y[0] == "after_split":
                        x = li(span("After ", timedeltaLitHtml(y[1]), " since the last event:"),
                               indented(x))
                    elif y[0] == "at_or_after_split":
                        x = li(span("On or after ", timedeltaLitHtml(y[1]), " since the last event:"),
                               indented(x))
                    else:
                        raise NotImplementedError
                elif y[0] in {"before", "within", "at", "after", "at_or_after"}:
                    if y[0] == "within":
                        x = li(span("Within ", timedeltaLitHtml(y[1]), " since the start of the contract:"),
                               indented(x))
                    elif y[0] == "at":
                        x = li(span("At ", timedeltaLitHtml(y[1]), " since the start of the contract:"),
                               indented(x))
                    elif y[0] == "before":
                        x = li(span("Before ", timedeltaLitHtml(y[1]), " since the start of the contract:"),
                               indented(x))
                    elif y[0] == "after":
                        x = li(span("After ", timedeltaLitHtml(y[1]), " since the start of the contract:"),
                               indented(x))
                    elif y[0] == "at_or_after":
                        x = li(span("On or after ", timedeltaLitHtml(y[1]), " since the start of the contract:"),
                               indented(x))
                    else:
                        raise NotImplementedError
            else:
                x = li(span("if ", str(rule.time_constraint)),
                       indented(x))

        return x

    def termHtml(term:Term) -> Any:
        if isinstance(term, FnApp):
            if term.head == ">":
                return span(termHtml(term.args[0]), " is greater than ", termHtml(term.args[1]))
            elif term.head == "<":
                return span(termHtml(term.args[0]), " is less than ", termHtml(term.args[1]))
            elif term.head == "≥" or term.head == ">=":
                return span(termHtml(term.args[0]), " is at least ", termHtml(term.args[1]))
            elif term.head == "==":
                return span(termHtml(term.args[0]), " = ", termHtml(term.args[1]))
            elif term.head == "check" or term.head == "cast":
                return termHtml(term.args[1])
            elif term.head == "/" or term.head == "*":
                return span(termHtml(term.args[0]), " ", term.head, " ", termHtml(term.args[1]))
            elif term.head == "ceil/":
                return span("⎡", termHtml(term.args[0]), " / ", termHtml(term.args[1]), "⎤")
            elif term.head == "min":
                return span("the minimum of ", termHtml(term.args[0]), " and ", termHtml(term.args[1]))
            elif term.head == "+" or term.head == "-":
                return span("( ", termHtml(term.args[0]), " ", term.head, " ", termHtml(term.args[1]), " )")
        elif isinstance(term, ActionBoundActionParam):
            return id2link(term.name, term.action.action_id)
        elif isinstance(term, StateVar):
            return id2link(term.name)
        elif isinstance(term, ContractParam):
            return id2link(term.name)
        elif isinstance(term, LocalVar):
            return id2link(term.name)
        return maybeNL(str(term))

    doc = html()

    doc.add(
        head(
            style(CSS)
        )
    )
    docbody = doc.add(body())
    docbody.add(title(f"{prog.filename}"), h1("Simple Agreement for Future Equity (SAFE)"))

    docbody.add(contract_params_section())

    docbody.add(state_vars_section())

    nlgsections : Any = {"root": []}

    for sit in prog.situations_iter():
        if sit.nlgsection not in nlgsections:
            nlgsections[sit.nlgsection] = []
        if sit.nlg:
            nlgsections[sit.nlgsection].append(sit)

            # add_situation(sit)
    for act in prog.actions_iter():
        if act.nlgsection not in nlgsections:
            nlgsections[act.nlgsection] = []
        if act.nlg:
            nlgsections[act.nlgsection].append(act)

    sections = docbody.add(div(cls="nomarkers"))
    for sect in nlgsections:
        if sect != "root":
            section = sections.add(li())
            section.add(h2(sect))
            section_items = section.add(indented())
        else:
            section = sections.add(div())
            # section.add(h2(sect))
            section_items = section.add(div())

        for thing in nlgsections[sect]:
            if isinstance(thing,Action):
                section_items.add(actionHtml(thing))
            else:
                section_items.add(situationHtml(thing))

    writeFile(outpath,str(doc))


