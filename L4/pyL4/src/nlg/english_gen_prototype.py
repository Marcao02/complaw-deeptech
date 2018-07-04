from distutils.file_util import write_file
from typing import Any, Dict, Optional, Match, List, NamedTuple
import re

from src.constants_and_defined_types import ContractParamId, FULFILLED_SITUATION_LABEL, ActionParamId, SituationId, \
    ActionId
from src.independent.util import castid
from src.independent.util_for_dicts import partitionBy
from src.independent.util_for_io import writeFile
from src.model.Action import Action
from src.model.ActionRule import ActionRule, PartyNextActionRule, EnvNextActionRule
from src.model.BoundVar import ActionBoundActionParam, StateVar, ContractParam, LocalVar, PrimedStateVar
from src.model.ContractParamDec import ContractParamDec
from src.model.L4Contract import L4Contract

import dominate #type:ignore
from dominate.tags import html_tag #type:ignore
from dominate.tags import * #type:ignore
from dominate.util import raw #type:ignore

from src.model.Literal import Literal

span : html_tag
h1 : html_tag
h2 : html_tag
h3 : html_tag
ul : html_tag
ol : html_tag
li : html_tag
a : html_tag
div : html_tag
br : html_tag
html : html_tag
head : html_tag
body : html_tag
style : html_tag
title : html_tag
p : html_tag

from src.model.Situation import Situation
from src.model.Sort import Sort
from src.model.Statement import Statement, StateVarAssign, LocalVarDec, StatementList
from src.model.Term import Term, FnApp

sort_descriptions = {
    "Δ$": "change in USD",
    "$": "USD",
    "Pos$": "positive USD",

    "Fraction(0,1]": "fraction in the range (0,1]",
    "Fraction[0,1)": "fraction in the range [0,1)",
    "ShareCnt": "number of shares",
    "ΔShareCnt": "change in number of shares",
    "PosShareCnt": "positive number of shares",

    "SharePrice": "price in USD/share",
    "PosSharePrice": "positive price in USD/share",

    "TimeDelta": "number of days",
}

CSS = """
.action_header,.situation_header {
    margin: .2%;
}
body {
    margin-bottom : 1000px;
}
.nomarkers {
    list-style-type: none;
}
.defined_term_intro {
    font-weight: bold;
}
.role {
    
}
.actionword {
    
}
.situationword {

}
.symbintro {
    color: darkgoldenrod;
}
.is_assignment {
    font-weight: bold;
}
.boiler {
    font-weight: bold;
}

.indent1 {
    
}
.indent2 {

}
.indent3 {

}
.indent4 {

}
.indent5 {

}
.indent6 {

}
"""

children_open_css = "fa-li fa fa-caret-down"
children_closed_css = "fa-li fa fa-caret-right"

SPACES_PER_TAB = 4

class ProseLine(NamedTuple):
    indent: int
    bullet: Optional[str]
    text: str

ProseText = List[ProseLine]


# def insert_raw_html_in_text_nodes(x:html_tag) -> None:
#     if isinstance(x,div):
#         x.get("text")
#     elif isinstance(x,li) or isinstance(x,ol):
#         print(x.get('text'))
#         for child in x.children:
#             insert_raw_html_in_text_nodes(child)
#     else:
#         print("Unsupported: ", x)


testtext = """
(a)	The {Company} is a corporation duly organized, validly existing and in good standing under the laws of the state of its incorporation, and has the power and authority to own, lease and operate its properties and carry on its business as now conducted.
(b)	The execution, delivery and performance by the {Company} of this instrument is within the power of the {Company} and, other than with respect to the actions to be taken when equity is to be issued to the Investor, has been duly authorized by all necessary actions on the part of the {Company}.
	This instrument constitutes a legal, valid and binding obligation of the {Company}, enforceable against the {Company} in accordance with its terms, except as limited by bankruptcy, insolvency or other laws of general application relating to or affecting the enforcement of creditors’ rights generally and general principles of equity.   
	To the knowledge of the {Company}, it is not in violation of 
	(i) its current certificate of incorporation or bylaws,
	(ii) any material statute, rule or regulation applicable to the {Company} or
	(iii) any material indenture or contract to which the {Company} is a party or by which it is bound,
	where, in each case, such violation or default, individually, or together with all such violations or defaults, could reasonably be expected to have a material adverse effect on the {Company}.
(c)	The performance and consummation of the transactions contemplated by this instrument do not and will not: 
	(i) violate any material judgment, statute, rule or regulation applicable to the {Company}; 
	(ii) result in the acceleration of any material indenture or contract to which the {Company} is a party or by which it is bound; or 
	(iii) result in the creation or imposition of any lien upon any property, asset or revenue of the {Company} or the suspension, forfeiture, or nonrenewal of any material permit, license or authorization applicable to the {Company}, its business or operations.
(d)	No consents or approvals are required in connection with the performance of this instrument, other than:  
	(i) the {Company}’s corporate approvals; 
	(ii) any qualifications or filings under applicable securities laws; and 
	(iii) necessary corporate approvals for the authorization of {Capital_Stock} issuable pursuant to Section 1.	
(e)	To its knowledge, the {Company} owns or possesses (or can obtain on commercially reasonable terms) sufficient legal rights to all patents, trademarks, service marks, trade names, copyrights, trade secrets, licenses, information, processes and other intellectual property rights necessary for its business as now conducted and as currently proposed to be conducted, without any conflict with, or infringement of the rights of, others."
(f) stuff
	(1) blah
	    (I) blah!
		(II) woo!
	(2) more stuff
	next
(g) stuff 2
	(1) blah
		(I) blah!
		(II) woo!
			(A) A
			(B) B
	(2) more stuff 2
	next 2
(h) stuff 3
	(0) uhm
	(1) blah
		(I) blah!
		(II) woo!
			(A) A
			(B) B	
	next 3
"""

testtext2 = """
(a) indented first list item 1
(b) indented first list item 2
"""

testtext3 = """
3.	Company Representations
	(a)	The {Company} is a corporation duly organized, validly existing and in good standing under the laws of the state of its incorporation, and has the power and authority to own, lease and operate its properties and carry on its business as now conducted.
	(b)	The execution, delivery and performance by the {Company} of this instrument is within the power of the {Company} and, other than with respect to the actions to be taken when equity is to be issued to the Investor, has been duly authorized by all necessary actions on the part of the {Company}.
		This instrument constitutes a legal, valid and binding obligation of the {Company}, enforceable against the {Company} in accordance with its terms, except as limited by bankruptcy, insolvency or other laws of general application relating to or affecting the enforcement of creditors’ rights generally and general principles of equity.   
		To the knowledge of the {Company}, it is not in violation of 
		(i) its current certificate of incorporation or bylaws,
		(ii) any material statute, rule or regulation applicable to the {Company} or
		(iii) any material indenture or contract to which the {Company} is a party or by which it is bound,
		where, in each case, such violation or default, individually, or together with all such violations or defaults, could reasonably be expected to have a material adverse effect on the {Company}.
	(c)	The performance and consummation of the transactions contemplated by this instrument do not and will not: 
		(i) violate any material judgment, statute, rule or regulation applicable to the {Company}; 
		(ii) result in the acceleration of any material indenture or contract to which the {Company} is a party or by which it is bound; or 
		(iii) result in the creation or imposition of any lien upon any property, asset or revenue of the {Company} or the suspension, forfeiture, or nonrenewal of any material permit, license or authorization applicable to the {Company}, its business or operations.
	(d)	No consents or approvals are required in connection with the performance of this instrument, other than:  
		(i) the {Company}’s corporate approvals; 
		(ii) any qualifications or filings under applicable securities laws; and 
		(iii) necessary corporate approvals for the authorization of {Capital_Stock} issuable pursuant to Section 1.
	(e)	To its knowledge, the {Company} owns or possesses (or can obtain on commercially reasonable terms) sufficient legal rights to all patents, trademarks, service marks, trade names, copyrights, trade secrets, licenses, information, processes and other intellectual property rights necessary for its business as now conducted and as currently proposed to be conducted, without any conflict with, or infringement of the rights of, others."

"""

testtext4 = """
3.	Company Representations
	(a)	The {Company} is a corporation duly organized, validly existing and in good standing under the laws of the state of its incorporation, and has the power and authority to own, lease and operate its properties and carry on its business as now conducted.
	(b)	The execution, delivery and performance by the {Company} of this instrument is within the power of the {Company} and, other than with respect to the actions to be taken when equity is to be issued to the Investor, has been duly authorized by all necessary actions on the part of the {Company}.
	This instrument constitutes a legal, valid and binding obligation of the {Company}, enforceable against the {Company} in accordance with its terms, except as limited by bankruptcy, insolvency or other laws of general application relating to or affecting the enforcement of creditors’ rights generally and general principles of equity.   
	To the knowledge of the {Company}, it is not in violation of 
	(i) its current certificate of incorporation or bylaws,
	(ii) any material statute, rule or regulation applicable to the {Company} or
	(iii) any material indenture or contract to which the {Company} is a party or by which it is bound,
	where, in each case, such violation or default, individually, or together with all such violations or defaults, could reasonably be expected to have a material adverse effect on the {Company}.
(c)	The performance and consummation of the transactions contemplated by this instrument do not and will not: 
	(i) violate any material judgment, statute, rule or regulation applicable to the {Company}; 
	(ii) result in the acceleration of any material indenture or contract to which the {Company} is a party or by which it is bound; or 
	(iii) result in the creation or imposition of any lien upon any property, asset or revenue of the {Company} or the suspension, forfeiture, or nonrenewal of any material permit, license or authorization applicable to the {Company}, its business or operations.
(d)	No consents or approvals are required in connection with the performance of this instrument, other than:  
	(i) the {Company}’s corporate approvals; 
	(ii) any qualifications or filings under applicable securities laws; and 
	(iii) necessary corporate approvals for the authorization of {Capital_Stock} issuable pursuant to Section 1.	
(e)	To its knowledge, the {Company} owns or possesses (or can obtain on commercially reasonable terms) sufficient legal rights to all patents, trademarks, service marks, trade names, copyrights, trade secrets, licenses, information, processes and other intellectual property rights necessary for its business as now conducted and as currently proposed to be conducted, without any conflict with, or infringement of the rights of, others."

4.	Investor Representations
	(a)	The {Investor} has full legal capacity, power and authority to execute and deliver this instrument and to perform its obligations hereunder. This instrument constitutes valid and binding obligation of the {Investor}, enforceable in accordance with its terms, except as limited by bankruptcy, insolvency or other laws of general application relating to or affecting the enforcement of creditors’ rights generally and general principles of equity.
	(b)	The {Investor} is an accredited investor as such term is defined in Rule 501 of Regulation D under {Securities_Act}. 
	The {Investor} has been advised that this instrument and the underlying securities have not been registered under {Securities_Act}, or any state securities laws and, therefore, cannot be resold unless they are registered under {Securities_Act} and applicable state securities laws or unless an exemption from such registration requirements is available. 
	The {Investor} is purchasing this instrument and the securities to be acquired by the {Investor} hereunder for its own account for investment, not as a nominee or agent, and not with a view to, or for resale in connection with, the distribution thereof, and the {Investor} has no present intention of selling, granting any participation in, or otherwise distributing the same. The {Investor} has such knowledge and experience in financial and business matters that the {Investor} is capable of evaluating the merits and risks of such investment, is able to incur a complete loss of such investment without impairing the {Investor}’s financial condition and is able to bear the economic risk of such investment for an indefinite period of time.

5.	Miscellaneous
	(a)	Any provision of this instrument may be amended, waived or modified only upon the written consent of the {Company} and the {Investor}.
	(b)	Any notice required or permitted by this instrument will be deemed sufficient when delivered personally or by overnight courier or sent by email to the relevant address listed on the signature page, or {48 hours} after being deposited in the U.S. mail as certified or registered mail with postage prepaid, addressed to the party to be notified at such party’s address listed on the signature page, as subsequently modified by written notice.
	(c)	The {Investor} is not entitled, as a holder of this instrument, to vote or receive dividends or be deemed the holder of Capital Stock for any purpose, nor will anything contained herein be construed to confer on the {Investor}, as such, any of the rights of a stockholder of the {Company} or any right to vote for the election of directors or upon any matter submitted to stockholders at any meeting thereof, or to give or withhold consent to any corporate action or to receive notice of meetings, or to receive subscription rights or otherwise until shares have been issued upon the terms described herein.
	(d)	Neither this instrument nor the rights contained herein may be assigned, by operation of law or otherwise, by either party without the prior written consent of the other; provided, however, that this instrument and/or the rights contained herein may be assigned without the {Company}’s consent by the {Investor} to any other entity who directly or indirectly, controls, is controlled by or is under common control with the {Investor}, including, without limitation, any general partner, managing member, officer or director of the {Investor}, or any venture capital fund now or hereafter existing which is controlled by one or more general partners or managing members of, or shares the same management company with, the {Investor}; and provided, further, that the {Company} may assign this instrument in whole, without the consent of the {Investor}, in connection with a reincorporation to change the {Company}’s domicile.
	(e)	In the event any one or more of the provisions of this instrument is for any reason held to be invalid, illegal or unenforceable, in whole or in part or in any respect, or in the event that any one or more of the provisions of this instrument operate or would prospectively operate to invalidate this instrument, then and in any such event, such provision(s) only will be deemed null and void and will not affect any other provision of this instrument and the remaining provisions of this instrument will remain operative and in full force and effect and will not be affected, prejudiced, or disturbed thereby.
	(f)	All rights and obligations hereunder will be governed by the laws of the State of {Governing_Law_Jurisdiction}, without regard to the conflicts of law provisions of such jurisdiction.

"""

# indented_text_to_html(testtext)
# indented_text_to_html(testtext2)
# indented_text_to_html(testtext3)
# indented_text_to_html(testtext4)
# exit()

def actiontitle(act:Action) -> Any:
    return h3(span(act.allowed_subjects[0],cls="role"),
              span(" action: ",cls="actionword"),
              act.nlg,
              id=act.action_id, cls="action_header")

def situationtitle(sit:Situation) -> Any:
    return h3(span("Scenario: ",cls="situationword"),
              sit.nlg,
              id=sit.situation_id, cls="situation_header")

def indented(x:Optional[Any] = None) -> Any:
    if x:
        return ul(x,cls="nomarkers")
    else:
        return ul(cls="nomarkers")

def one_indented(x:Any) -> Any:
    return indented(li(x))

def gen_english(prog:L4Contract, outpath:str) -> None:
    bulletre = re.compile("\s*(" + "|".join(("\w\.", "\(\w\)", "\w\)",
                                             "i+\.", "i+\)", "\(i+\)",
                                             "I+\.", "I+\)", "\(I+\)")) + ")(.*)$")
    indentre = re.compile("((?:\\t*)|(?: *))[^\s]")

    def indented_text_to_html(inp: str) -> html_tag:
        def mkli(s: str) -> html_tag:
            # return li(span(cls="collapsible-list fa-li fa fa-caret-down"), insert_refs(s), cls="nomarkers")
            return li(insert_refs(s), cls="nomarkers")

        def mkdiv(s: str) -> html_tag:
            return div(insert_refs(s))

        def mkol(s: Optional[str] = None) -> html_tag:
            return ol(insert_refs(s), cls="nomarkers") if s else ol(cls="nomarkers")

        """
        Will first parse lines to have form
        (indent amount: int, bullet: Optional[str], text: str)

        THIS IS NOT CORRECT:
            A line in inp that starts with k tabs and then has a number/delimiter
                and follows a line with k-1 tabs, will create a new <ol>
                and follows a line with k tabs, will create a new <li>
                and follows a line with k+1 tabs, will complete the current <ol>
            A line in inp that starts with k tabs, where k-1 is the indent of the most recent <ol>, and then has something other than a number/delimiter, creates a new <div> from the single line
        """
        rawlines = inp.split("\n")

        def rawline_to_tuple(rawline: str) -> Optional[ProseLine]:
            m = indentre.match(rawline)
            if m:
                indent = len(m[1])
                if indent > 0 and m[1][0] == ' ':
                    indent = int(indent / SPACES_PER_TAB)
                m = bulletre.match(rawline)
                bullet, text = (m[1], m[2]) if m else (None, rawline[indent:])
                return ProseLine(indent, bullet, text)
            return None

        tups = tuple(map(rawline_to_tuple, rawlines))
        # for x in tups:
        #     print(x)

        rv = div()
        cur = rv
        cur_list_indent = -1

        for x in tups:
            if not x:
                continue
            if x.bullet:
                # if x.indent == cur_list_indent + 1:
                if x.indent >= cur_list_indent + 1:
                    # new deeper list
                    cur = cur.add(mkol())
                    # first bullet of new list is given in the same raw text line:
                    cur = cur.add(mkli(x.bullet + " " + x.text))
                    # cur_list_indent = cur_list_indent + 1
                    cur_list_indent = x.indent
                elif x.indent == cur_list_indent:
                    # new bullet in same list
                    cur = cur.parent  # pop out of the li only

                    cur = cur.add(mkli(x.bullet + " " + x.text))  # pop into a new li
                else:
                    assert x.indent < cur_list_indent
                    # back into a higher list
                    for i in range(cur_list_indent - x.indent):
                        # pop out of a li ol pair
                        cur = cur.parent.parent
                    # add the next li
                    cur = cur.add(mkli(x.bullet + " " + x.text))
                    cur_list_indent = x.indent

            else:  # Line break without a new bullet
                if x.indent < cur_list_indent + 1:
                    # this non-bullet line break ends a list
                    for i in range(cur_list_indent + 1 - x.indent):
                        # pop out of li ol pair
                        cur = cur.parent.parent
                    cur_list_indent = x.indent - 1
                    cur.add(mkdiv(x.text))
                else:
                    # this non-bullet line break does not end a list
                    # so we're in the same li
                    cur.add(mkdiv(x.text))

        # print(type(rv))
        # print(rv)
        return rv

    def sitid2link(sitid:str) -> html_tag:
        sit = prog.situation(castid(SituationId,sitid))
        return a(sit.nlg, href=f"#{sitid}")
    def actid2link(actid:str) -> html_tag:
        act = prog.action(castid(ActionId,actid))
        return a(act.nlg, href=f"#{actid}")
    def id2link(display_name:str, ctx:Optional[str] = None) -> html_tag:
        linkid = display_name if display_name[-1] != "'" else display_name[:-1]
        if ctx:
            return a(maybeNL(display_name), href=f"#{ctx}.{linkid}")
        else:
            return a(maybeNL(display_name), href=f"#{linkid}")

    def intro(nameid: str) -> html_tag:
        return span(maybeNL(nameid), id=nameid, cls="symbintro")
    def actionparamIntro(pname: str, sort: Sort, actid:str) -> html_tag:
        return li(intro(pname), ", which is a " + sortHtml(sort), id=f"{actid}.{pname}")

    def maybeNL(s:str) -> str:
        return prog.nlg_names[s] if s in prog.nlg_names else s

    def contract_params_section() -> html_tag:
        params = prog.contract_params
        rv = div()
        rv.add(div("The parameters to the contract are:"))
        cpul = rv.add(indented())
        for para,dec in params.items():
            cpul.add( li( intro(para), ", which is a ", f"{sortHtml(dec.sort)}.") )
        return rv

    def state_vars_section() -> html_tag:
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

    def sortHtml(sort:Sort) -> str:
        return sort_descriptions[str(sort)]

    def situationHtml(sit:Situation, is_anon=False) -> html_tag:
        # show two rules with the same action-enabled guard together
        rules_by_enabled_guard = partitionBy(lambda x: x.entrance_enabled_guard, sit.action_rules())
        sitsec = li()
        sitsec.add(situationtitle(sit))
        rules = sitsec.add(indented())
        for guardkey,rulelist in rules_by_enabled_guard.items():
            if guardkey != "None":
                theguard = rulelist[0].entrance_enabled_guard
                assert theguard is not None
                rulegroup = rules.add(li(span("if ", termHtml(theguard)))).add(indented())
                for rule in rulelist:
                    rulegroup.add(ruleHtml(rule))
            else:
                # no action-enabled guard
                for rule in rulelist:
                    rules.add(ruleHtml(rule))

        return sitsec

    def actionHtml(act: Action) -> html_tag:
        # actsec = indented()
        # container.add(actsec)
        rv = li(actiontitle(act))
        actcontents = rv.add(indented())
        if act.param_names and len(act.param_names) > 0:
            actcontents.add(div(act.allowed_subjects[0] + " must provide:"))
            actcontents.add(actionparamsHtml(act.param_sorts_by_name, act.action_id))
            actcontents.add(br())
        if act.state_transform:
            # actcontents.add(div("We calculate:"))
            # statetrans = actcontents.add(indented())
            statetrans = actcontents.add(div())
            statetrans.add(blockHtml(act.state_transform.statements))
            actcontents.add(br())

        if act.following_anon_situation:
            actcontents.add(situationHtml(act.following_anon_situation, True))
        elif act.dest_situation_id == FULFILLED_SITUATION_LABEL:
            actcontents.add(div("Contract fulfilled."))
        else:
            actcontents.add(div("Go to ", sitid2link(act.dest_situation_id)))
        return rv

    def actionparamsHtml(d:Dict[ActionParamId, Sort], actid:str) -> html_tag:
        rv = indented()
        for pname,sort in d.items():
            rv.add(actionparamIntro(pname,sort,actid))
            # rv.add(li(pname + ", which is a " + sortHtml(sort)))
        return rv

    def blockHtml(block:StatementList) -> html_tag:
        rv = div()
        for statement in block:
            rv.add(statementHtml(statement))
        return rv

    def statementHtml(statement:Statement) -> html_tag:
        if isinstance(statement,StateVarAssign):
            rhs = statement.value_expr
            if isinstance(rhs, FnApp) and rhs.fnsymb_name == "-":
                return div("Reduce ", id2link(statement.varname), " by ", termHtml(rhs.args[1]))
            if isinstance(rhs, FnApp) and rhs.fnsymb_name == "+":
                return div("Increase ", id2link(statement.varname), " by ", termHtml(rhs.args[1]))
            else:
                return div(id2link(statement.varname), span(" is ", cls="is_assignment"),
                           termHtml(statement.value_expr))
                # return div(id2link(statement.varname), span("  to:",cls="is_assignment"), one_indented(termHtml(statement.value_expr)))
            # return div(id2link(statement.varname), span(" by:"), one_indented(termHtml(statement.value_expr)))
        elif isinstance(statement,LocalVarDec):
            return div(intro(statement.varname), " is ", termHtml(statement.value_expr))
        # elif isinstance(statement,LocalVarDec):
        #     return div(intro(statement.varname), ", a ", sortHtml(statement.sort), "  to:", one_indented(termHtml(statement.value_expr)))
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

    def ruleHtml(rule:ActionRule) -> html_tag:
            
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
                return span("the lesser of ", termHtml(term.args[0]), " and ", termHtml(term.args[1]))
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
        elif isinstance(term,Literal):
            return str(term)
        elif isinstance(term, PrimedStateVar):
            return span(id2link(term.name))
        print("fallback case in termHtml for " + str(term) + " of type " + str(type(term)))
        return maybeNL(str(term))

    def mk_terms_in_prose_regexp() -> Any:
        s = ("|".join("(?:\{" + x + "\})|(?:\{val\s" + x + "\})|(?:\{def\s" + x + "\})" for x in prog.nlg_definitions) + "|" +
             "|".join("(?:\{" + x + "\})|(?:\{val\s" + x + "\})|(?:\{def\s" + x + "\})" for x in prog.nlg_names) + "|" +
             "|".join("(?:\{" + x + "\})|(?:\{val\s" + x + "\})|(?:\{def\s" + x + "\})" for x in prog.contract_params.keys()) + "|" +
             "|".join("(?:\{" + x + "\})|(?:\{val\s" + x + "\})|(?:\{def\s" + x + "\})" for x in prog.contract_params_nonoperative.keys()))
        # don't know why s starting with "|" was/is happening.
        if s[0] == "|":
            s = s[1:]

        return re.compile(s)

    def _get_val(s:str) -> Any:
        if s in prog.contract_params:
            return str(prog.contract_params[castid(ContractParamId,s)].value_expr)
        elif s in prog.contract_params_nonoperative:
            return str(prog.contract_params_nonoperative[s])
        elif s in prog.nlg_names:
            return str(prog.nlg_names[s])
        elif s in prog.nlg_definitions:
            return str(prog.nlg_definitions[s])

    def to_link(s:str, display:Optional[str] = None) -> str:
        return f"<a href='#{s}'>{display}</a>" if display else f"<a href='#{s}'>{s}</a>"

    def to_link_target(s:str) -> str:
        return f"<span id='{s}' class='defined_term_intro'>{s}</span>"

    def replacer(mo:Match) -> str:
        if mo[0][1:-1].startswith("val"):
            ident = mo[0][5:-1]
            return to_link(ident, _get_val(ident))
        elif mo[0][1:-1].startswith("def"):
            return to_link_target(mo[0][5:-1])
        return to_link(mo[0][1:-1])

    nltermsregexp = mk_terms_in_prose_regexp()
    def insert_refs(s:str) -> Any:
        rv = nltermsregexp.sub(replacer,s)
        # print(rv)
        return raw(rv)

    def l4_name_to_nl(s:str) -> html_tag:
        return span(raw(s.replace("_"," ")), cls="defined_term_intro")

    def nl_defn_html(nlterm:str, defn:str) -> html_tag:
        return li(l4_name_to_nl(nlterm), " - ", insert_refs(defn), id=nlterm, _class="triggers-tooltip nomarkers")
        # return li(l4_name_to_nl(nlterm) + " - ", insert_refs(defn, nltermsregexp), id=nlterm,_class="triggers-tooltip")

    def source_prose_to_html(s:str) -> html_tag:
        return indented_text_to_html(s)

    def section_heading(section_title:str) -> html_tag:
        return h1(section_title)

    def code_group_heading(title:str) -> html_tag:
        return h2(title)

    def contract_logic() -> html_tag:
        # Ex: the group ids in SAFE example are
        # Events/Scenarios Relevant in All Cases
        # Equity Financing
        # IPO or Change of Control
        # Dissolution
        grouped_situations_and_events: Any = {"root": []}
        for sit in prog.situations_iter():
            if sit.nlgsection not in grouped_situations_and_events:
                grouped_situations_and_events[sit.nlgsection] = []
            if sit.nlg:
                grouped_situations_and_events[sit.nlgsection].append(sit)

                # add_situation(sit)
        for act in prog.actions_iter():
            if act.nlgsection not in grouped_situations_and_events:
                grouped_situations_and_events[act.nlgsection] = []
            if act.nlg:
                grouped_situations_and_events[act.nlgsection].append(act)

        code_groups = ol(cls="nomarkers")
        for group_heading in grouped_situations_and_events:
            if group_heading != "root":
                code_group = code_groups.add(li())
                # code_group = code_groups.add(div())
                code_group.add(code_group_heading(group_heading))
                code_group_items = code_group.add(indented())
            else:
                code_group = code_groups.add(div())
                # code_group.add(h2(sect))
                code_group_items = code_group.add(div())

            for thing in grouped_situations_and_events[group_heading]:
                if isinstance(thing, Action):
                    code_group_items.add(actionHtml(thing))
                else:
                    code_group_items.add(situationHtml(thing))

        return code_groups

    def html_str_from_source(prog:L4Contract#, structure:List[str]
                                  ) -> body:
        doc = html()
        doc.add(
            head(
                style(CSS)
                # ,raw("""<link rel = "stylesheet" href = "../font-awesome-4.4.0/css/font-awesome.min.css">""")
                # script(src="https://code.jquery.com/jquery-3.3.1.min.js"),
                # script(src="https://cdnjs.cloudflare.com/ajax/libs/qtip2/3.0.3/jquery.qtip.js"),
                # script(src="for_legalese_html_nlg.js")
            )
        )
        docbody = doc.add(body())
        docbody.add(title(f"{prog.filename}"), h1("Simple Agreement for Future Equity (SAFE)"))

        # "@Title",
        # "AfterTitle",
        # "@ContractParams",
        # "@StateVars",
        # [
        #     "@ContractLogic",
        #     "@Definitions",
        #     "3. Company Representatons",
        #     "4. Investor Representatons",
        #     "5. Miscellaneous"
        # ]

        if "AfterTitle" in prog.nlg_sections:
            docbody.add(source_prose_to_html(prog.nlg_sections["AfterTitle"]))
            docbody.add(br())

        docbody.add(contract_params_section())

        docbody.add(state_vars_section())

        docbody.add(section_heading("1."))

        docbody.add(contract_logic())

        # -----------prose stuff-----------
        docbody.add(section_heading("2. Definitions"))
        defns_html = ol()
        docbody.add(defns_html)
        for nlterm, defn in prog.nlg_definitions.items():
            defns_html.add(nl_defn_html(nlterm, defn))

        for prose_key, prose in prog.nlg_sections.items():
            # docbody.add(p(raw(insert_refs(prose,nltermsregexp))))
            if prose_key != "AfterTitle":
                docbody.add(section_heading(prose_key))
                docbody.add(source_prose_to_html(prose))
        # -----------end prose stuff-----------

        return doc

    # doc_structure = [
    #     "@Title",
    #     "AfterTitle",
    #     "@ContractParams",
    #     "@StateVars",
    #     [
    #     "@ContractLogic",
    #     "@Definitions",
    #     "3. Company Representatons",
    #     "4. Investor Representatons",
    #     "5. Miscellaneous"
    #     ]
    # ]

    generated_html = html_str_from_source(prog)
    # print(type(generated_html))
    writeFile(outpath,str(generated_html))