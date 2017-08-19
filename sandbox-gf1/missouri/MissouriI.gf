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

    --
    -- test 01
    --
  lin
    
    eating = mkEating
      (mkN "eating") -- of the
      (mkNP and_Conj
         -- it would be nice for the the_Det to live here,
         -- but we can't seem to Det -> NP -> mkNP
         (mkNP the_Det (mkCN (mkN "bacon")))
         (mkNP aPl_Det (mkCN (mkN "egg"))));
              
  oper
    mkEating : N -> NP -> NP =
      \eating, baconAndEggs ->
      mkNP the_Det
         (mkCN (mkN2 eating) -- of the
            baconAndEggs);

  lin               
    obesity = table {
      LexDeontic.Oblig => mkObesity some_Det;
      LexDeontic.Forb  => mkObesity  any_Det;
      LexDeontic.Perm  => mkObesity some_Det
      };

  oper
    mkObesity : Det -> VP = \det ->
      mkConsequence the_Det (mkN "effect") (mkV2 "cause") det (mkN "obesity");
    
    mkConsequence : Det -> N   -> V2 -> Det -> N       -> VP =
                   \the,effect,causing,some,  obesity ->
      (mkVP have_V2
         (mkNP the
            (mkCN (mkN2 effect) -- of
               (ExtensionsEng.GerundNP (mkVP causing
                                          (mkNP some
                                             (mkCN obesity)))))));
    

    --
    -- test 02
    --
    -- The repealing of the sections and law repealed by this law
    --   shall not have the effect of
    --   abating, nullifying, suspending or vitiating
    --   any public road district
    --   or any proceedings by any such public road district.

    


    --
    -- common
    --
  lin
    
    mkReactionRule event deon consequent =
      mkText (mkS (mkTemp presentTense simultaneousAnt)
                deon.pol
                (mkCl
                   event
                   deon.vv
                   (consequent ! deon.d)));


  oper
    any_Det  = mkDet (ParadigmsEng.mkQuant "any"  "any");
    some_Det = mkDet (ParadigmsEng.mkQuant "some" "some");

}
