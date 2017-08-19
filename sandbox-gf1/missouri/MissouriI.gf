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
    
    eating = mkEvent
      (mkN "eating") -- of the
      (mkNP and_Conj
         -- it would be nice for the the_Det to live here,
         -- but we can't seem to Det -> NP -> mkNP
         (mkNP the_Det (mkCN (mkN "bacon")))
         (mkNP aPl_Det (mkCN (mkN "egg"))));
              
    obesity = table {
      LexDeontic.Oblig => mkObesity some_Det;
      LexDeontic.Forb  => mkObesity  any_Det;
      LexDeontic.Perm  => mkObesity some_Det
      };

  oper
    mkObesity : Det -> VP = \det ->
      mkConsequence the_Det (mkN "effect") (mkV2 "cause") det (mkN "obesity");
    
    
    --
    -- test 02
    --
    -- The repealing of the sections and law repealed by this law
    --   shall not have the effect of
    --   abating, nullifying, suspending or vitiating
    --   any public road district
    --   or any proceedings by any such public road district.

    -- PhrUtt NoPConj (UttS (UseCl (TTAnt TPast ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumPl) (UseN boy_N)) (AdvVP (ComplV2 love_V2 (MassNP (UseN mother_N))) by_Adv)))) NoVoc

  lin    
    repealing = mkEvent
      (mkN "repealing") -- of the
      (mkNP and_Conj
         (mkNP aPl_Det (mkCN (mkN "section")))
         (mkNP aPl_Det (mkCN (mkN "law"))));

    abating = table {
      LexDeontic.Oblig => mkAbating some_Det;
      LexDeontic.Forb  => mkAbating  any_Det;
      LexDeontic.Perm  => mkAbating some_Det
      };

  oper
    mkAbating : Det -> VP = \det ->
      mkConsequence the_Det (mkN "effect") (mkV2 "abate") det (mkN "proceeding");
    
    
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
    mkEvent : N -> NP -> NP =
      \eating, baconAndEggs ->
      mkNP the_Det
         (mkCN (mkN2 eating) -- of the
            baconAndEggs);

    mkConsequence : Det -> N   -> V2 -> Det -> N       -> VP =
                   \the,effect,causing,some,  obesity ->
      (mkVP have_V2
         (mkNP the -- TODO：turn this into a quantifier
            (mkCN (mkN2 effect) -- of
               (ExtensionsEng.GerundNP (mkVP causing
                                          (mkNP some
                                             (mkCN obesity)))))));

    any_Det  = mkDet (ParadigmsEng.mkQuant "any"  "any");
    some_Det = mkDet (ParadigmsEng.mkQuant "some" "some");

}
