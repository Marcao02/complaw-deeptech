
-- functor for Gruter L4

incomplete concrete GruterI of Gruter =
  open Syntax, Sentence, LexGruter, LexDeontic
  in {
  lincat
    Contract = Text;
    WhenPredicate = Adv;
    Party = NP;
    Deontic = Deon;
    Action = VP;
    ActionExp = Cl;
    ActionKind = { }; -- dependent type disambiguation
  lin
    Clause when party deon kind act actexp =
      let main_act : S = 
            mkS (mkTemp presentTense simultaneousAnt)
            deon.pol
            (mkCl party deon.vv act)
      in
      mkText (ExtAdvS when
               (case deon.d of {
                 Oblig => RelS   main_act (mkRS (mkRCl actexp));
                 _     => SSubjS main_act where_Subj (mkS actexp)
               })
      );

    Deliver_Act = mkVP P_deliver_V2 P_things_NP;
    Deliver_Exp = mkCl P_things_NP (mkAP P_hot_A);

    Party_A = partyname P_Party_A;
    Party_B = partyname P_Party_B;
    Party_C = partyname P_Party_C;

    Pay_Act = mkVP P_pay_V;
    Pay_Exp = mkCl (mkNP the_Det P_pay_N) P_correct_A;

    Shall   = P_Shall;
    MustNot = P_MustNot;
    May     = P_May;

    Always   = mkAdv   if_Subj (mkS (mkCl P_the_sun  P_shines));
    BlueMoon = mkAdv when_Subj (mkS (mkCl P_the_moon P_blue));

    -- these are only used for disambiguating dependent types
    Pay_Kind, Deliver_Kind = <>;
}
