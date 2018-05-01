
-- specifies the NL-specific values of lexical/phrasal elements

instance LexPhoenixEng of LexPhoenix =
  open SyntaxEng, ParadigmsEng, IrregEng, ExtraEng,
       ResParty
    in {
  oper
    may       = may_VV;
    pay_V     = mkV "pay";
  oper
    partyname p = mkPN (case p.order of {
                             FnFirst => (p.prenom ++ p.surname )
                           ; SnFirst => (p.surname ++ p.prenom )
                            });
    
--    Before_T = mkAdv before_Prep (mkNP (mkN "tomorrow"));

    P_deliver_V2 = mkV2 "deliver";
    P_things_N   = mkN "things";
    P_correct_A  = mkA "correct";
    P_pay_N      = mkN "payment";

}
