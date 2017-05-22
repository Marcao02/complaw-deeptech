
-- specifies the NL-independent types of lexical/phrasal elements

interface LexPhoenix = ResParty **
                    open Syntax
  in {
  oper
    partyname    : Party -> PN;
    P_deliver_V2 : V2;
    P_things_N   : N;
    P_correct_A  : A;
    P_pay_N      : N;
}
