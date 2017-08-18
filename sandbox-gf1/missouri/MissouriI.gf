--# -path=.:present

incomplete concrete MissouriI of Missouri =
  GruterI **
  open SyntaxEng, ParadigmsEng, Prelude in
  {
  lincat
    ReactionRule = Text;
    Event = NP;
    Consequent = VP;

  oper
    LPP : P_Party = {
       prenom  = "Winston";
       surname = "Churchill";
       order   = FnFirst };

  lin
    Missouri_Party = partyname LPP;
    -- ontology of when-predicate conditions
    Missouri_When   = P_by_default;
    Win_Kind      = <>;
    Win_Act       = mkVP L_win_V;
    Win_Exp       = L_win_exp;

    eating = mkNP the_Det (mkCN
                             (mkN2 (mkN "eating"))
                             (mkNP and_Conj -- it would be nice for the the_Det to live here, but we can't seem to Det -> NP -> mkNP
                                (mkNP the_Det (mkCN (mkN "bacon")))
                                (mkNP aPl_Det (mkCN (mkN "egg")))))
                             ;

    obesity = (mkVP have_V2
                 (mkNP the_Det
                    (mkCN
                       (mkN2 (mkN "effect"))
                       (mkNP (mkN "causing obesity")))));
    
    mkReactionRule event deon consequent =
      mkText (mkS (mkTemp presentTense simultaneousAnt)
                deon.pol
                (mkCl
                   event
                   deon.vv
                   consequent));

                                  -- NP  have the effect of causing obesity
                                  
    
}
                                     