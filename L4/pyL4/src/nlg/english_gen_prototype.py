from distutils.file_util import write_file
from typing import Any, Dict

from src.constants_and_defined_types import ContractParamId
from src.independent.util_for_io import writeFile
from src.model.Action import Action
from src.model.ActionRule import ActionRule, PartyNextActionRule, EnvNextActionRule
from src.model.Block import Block
from src.model.ContractParamDec import ContractParamDec
from src.model.L4Contract import L4Contract

import dominate
from dominate.tags import *

from src.model.Situation import Situation
from src.model.Sort import Sort
from src.model.Statement import Statement
from test.active_examples import EXAMPLES_HTML_ROOT

sort_descriptions = {
    "$": "USD amount",
    "Pos$": "positive USD amount",
    "Fraction(0,1]": "fraction in the range (0,1]",
    "ShareCnt": "number of shares",
    "PosShareCnt": "positive number of shares",
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
"""

def actiontitle(act:Action) -> Any:
    return h3(span(act.allowed_subjects[0],cls="role"),
              span(" action ",cls="actionword"),
              act.nlg,
              id=act.action_id)
def situationtitle(sit:Situation) -> Any:
    return h3(span("Scenario: ",cls="situationword"),
              sit.nlg,
              id=sit.situation_id)

def gen_english(prog:L4Contract, outpath:str) -> str:
    def sitid2link(sitid) -> Any:
        sit = prog.situation(sitid)
        return a(sit.nlg, href=f"#{sitid}")

    def actid2link(actid) -> Any:
        act = prog.action(actid)
        return a(act.nlg, href=f"#{actid}")

    doc = html()

    doc.add(
        head(
            style(CSS)
        )
    )
    docbody = doc.add(body())
    docbody.add(title("prog.filename"), h1("Simple Agreement fo Future Equity (SAFE)"))

    def svar(s: str) -> Any:
        return b(s)

    def add_contract_params_section() -> None:
        params = prog.contract_params
        docbody.add(div("The parameters to the contract are:"))
        cpul = docbody.add(ul(cls="nomarkers"))
        for para,dec in params.items():
            cpul.add( li( svar(para), f" which is a {sort_descriptions[str(dec.sort)]}.") )

    def situationHtml(sit:Situation) -> Any:
        sitsec = li()
        sitsec.add(situationtitle(sit))
        rules = sitsec.add(ul(cls="nomarkers"))
        for rule in sit.action_rules():
            rules.add(ruleHtml(rule))
        return sitsec

    def actionHtml(act: Action) -> Any:
        # actsec = ul(cls="nomarkers")
        # container.add(actsec)
        rv = li(actiontitle(act))
        actcontents = rv.add(ul(cls="nomarkers"))
        if len(act.param_names) > 0:
            actcontents.add(div(act.allowed_subjects[0] + " must provide:"))
            actcontents.add(paramsHtml(act.param_sorts_by_name))
        if act.state_transform:
            actcontents.add(div("Define:"))
            statetrans = actcontents.add(ul(cls="nomarkers"))
            statetrans.add(blockHtml(act.state_transform.statements))
        actcontents.add(div("Go to ", actid2link(act.action_id)))
        return rv

    def paramsHtml(d:Dict[str,Sort]) -> Any:
        rv = ul(cls="nomarkers")
        for pname,sort in d.items():
            rv.add(li(pname + ", which is a " + sort_descriptions[str(sort)]))
        return rv


    def blockHtml(block:Block) -> Any:
        rv = div()
        for statement in block:
            rv.add(statementHtml(statement))
        return rv

    def statementHtml(statement:Statement) -> Any:
        return div(str(statement))



    def ruleHtml(rule:ActionRule):
        if isinstance(rule, PartyNextActionRule):
            return li(
                span(rule.role_ids[0]),
                span(f" {rule.deontic_keyword} "),
                actid2link(rule.action_id)
            )
        elif isinstance(rule, EnvNextActionRule):
            return li(
                span("Go to "),
                actid2link(rule.action_id)
            )

        else:
            raise NotImplementedError

    add_contract_params_section()

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
            section_items = section.add(ul(cls="nomarkers"))
        else:
            section = sections.add(div())
            # section.add(h2(sect))
            section_items = section.add(div())

        for thing in nlgsections[sect]:
            if isinstance(thing,Action):
                section_items.add(actionHtml(thing))
            else:
                section_items.add(situationHtml(thing))

    print(doc)
    writeFile(outpath,str(doc))


