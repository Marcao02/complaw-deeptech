
-- functor for Gruter L4

incomplete concrete GruterI of Gruter =
  open Syntax, Sentence, LexGruter, LexDeontic, LexParty
  in {
  lincat
    Contract = Text;
    WhenPredicate = Adv;
    Party = NP;
    Deontic = Deon;
    Action = VP;
    ActionExp = Cl;
    ActionKind = { }; -- dependent type disambiguation

  oper
    -- PARTY CONFIGURATION
    P_Party_A : P_Party = { prenom="Alice" ; surname="Andromeda" ; order = FnFirst }; 
    P_Party_B : P_Party = { prenom="Bobo"  ; surname="Bai"       ; order = SnFirst }; 
    P_Party_C : P_Party = { prenom="Carol" ; surname="Centaurus" ; order = FnFirst }; 

  lin
    -- PARTY LOGIC -- see LexParty
    Party_A = partyname P_Party_A;
    Party_B = partyname P_Party_B;
    Party_C = partyname P_Party_C;

  lin
    -- DEONTIC LOGIC -- see LexDeontic
    Shall   = D_Shall;
    MustNot = D_MustNot;
    May     = D_May;

  lin
    -- main linearization
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

    -- ontology of actions and action-expressions
    Deliver_Act = mkVP P_deliver_V2 P_things_NP;
    Deliver_Exp = mkCl P_things_NP (mkAP P_hot_A);

    Pay_Act = mkVP P_pay_V;
    Pay_Exp = mkCl (mkNP the_Det P_pay_N) P_correct_A;

    -- these are only used for disambiguating dependent types
    Pay_Kind, Deliver_Kind = <>;

    -- ontology of when-predicate conditions
    Always   = mkAdv   if_Subj (mkS (mkCl P_the_sun  P_shines));
    BlueMoon = mkAdv when_Subj (mkS (mkCl P_the_moon P_blue));

}
