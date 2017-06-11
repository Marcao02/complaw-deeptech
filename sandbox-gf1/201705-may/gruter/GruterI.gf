
-- functor for Gruter L4

incomplete concrete GruterI of Gruter =
  -- these appear to be imported by the GruterIEng functor instantiation, so
  -- we don't strictly need to open them here ... it still works!
  --  open Syntax, Sentence, LexGruter, LexDeontic, LexParty, LexL4 in
  {
  lincat
    Contract = Text;
    WhenPredicate = Adv;
    Party = NP;
    Deontic = Deon;
    Action = VP;
    ActionExp = Cl;
    ActionKind = { }; -- dependent type disambiguation

  lin
    Default_Party = partyname { prenom="David" ; surname="Default" ; order = FnFirst }; 
    Default_ActionKind = <>;
    Default_Act = P_default_act;
    Default_Exp = P_default_exp;
    
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

    -- ontology of when-predicate conditions
    Default_When   = P_by_default;

}
