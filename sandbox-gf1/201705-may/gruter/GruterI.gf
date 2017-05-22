
-- functor for Gruter L4

incomplete concrete GruterI of Gruter =
  open Syntax, Sentence, LexGruter
  in {
  lincat
    Contract = Utt;
    WhenPredicate = Adv;
    Party = NP;
    Deontic = Deon;
    Action = VP;
    ActionExp = Cl;
  lin
    Clause when party deon act actexp =
      mkUtt (ExtAdvS when
               (mkS
                  (mkTemp presentTense simultaneousAnt)
                  deon.pol
                  (mkCl party deon.vv act))
      );

    Deliver_Act = mkVP P_deliver_V2 (mkNP (P_things_N));
    Deliver_Exp = mkCl (mkNP P_things_N) (mkAP P_correct_A);

    Party_A = partyname P_Party_A;
    Party_B = partyname P_Party_B;
    Party_C = partyname P_Party_C;

    Pay_Act = mkVP P_pay_V;
    Pay_Exp = mkCl (mkNP P_pay_N) P_correct_A;

    Shall   = P_Shall;
    MustNot = P_MustNot;
    May     = P_May;
    
    Always = mkAdv if_Subj (mkS (mkCl P_the_sun P_shines));
    BlueMoon = mkAdv when_Subj (mkS (mkCl P_the_moon P_blue));
    
               
}
