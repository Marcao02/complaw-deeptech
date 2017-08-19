--# -path=.:present

incomplete concrete MissouriI of Missouri =
  GruterI **
  open SyntaxEng, ParadigmsEng, Prelude, (RE=ResEng), (EE=ExtraEng), ExtensionsEng in
  {
  lincat
    ReactionRule = Text;
    Event = NP;
    Consequent = LexDeontic.DOp => VP;

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
                                -- it would be nice for the the_Det to live here,
                                -- but we can't seem to Det -> NP -> mkNP
                                (mkNP the_Det (mkCN (mkN "bacon")))
                                (mkNP aPl_Det (mkCN (mkN "egg")))))
                             ;

    mkReactionRule event deon consequent =
      mkText (mkS (mkTemp presentTense simultaneousAnt)
                deon.pol
                (mkCl
                   event
                   deon.vv
                   (consequent ! deon.d)));

    obesity = table {
      LexDeontic.Oblig => monobesity some_Det ;
      LexDeontic.Forb  => monobesity any_Det  ;
      LexDeontic.Perm  => monobesity some_Det
      };


  oper
    any_Det  = mkDet (ParadigmsEng.mkQuant "any"  "any");
    some_Det = mkDet (ParadigmsEng.mkQuant "some" "some");

    monobesity : Det -> VP = \det ->
      (mkVP have_V2
         (mkNP the_Det
            (mkCN
               (mkN2 (mkN "effect"))
               (ExtensionsEng.GerundNP
                  (mkVP
                     (mkV2 "cause")
                     (mkNP det (mkCN (mkN "obesity"))))))));
    
}
