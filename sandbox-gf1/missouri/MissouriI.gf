--# -path=.:present

incomplete concrete MissouriI of Missouri =
  GruterI **
  open SyntaxEng, ParadigmsEng, Prelude in
  {
  lincat
    ReactiveRule = Text;
    Happening = NP;

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
                             (mkNP and_Conj
                                (mkNP the_Det (mkCN (mkN "bacon")))
                                (mkNP aPl_Det (mkCN (mkN "egg")))))
                             ;
    
    mkReactiveRule happening1 deon happening2 =
      mkText (mkS (mkTemp presentTense simultaneousAnt)
                deon.pol
                (mkCl
                   happening1
                   deon.vv
                   (mkVP (mkV "cause obesity"))));

    
}
                                     