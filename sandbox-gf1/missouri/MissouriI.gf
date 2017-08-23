--# -path=.:present:librarybrowser

incomplete concrete MissouriI of Missouri =
  GruterI **
  open SyntaxEng, ParadigmsEng, Prelude, (RE=ResEng), (EE=ExtraEng),
  ExtensionsEng
  in
  {
  lincat
    ReactionRule = Text;
    Event = NP;
    Consequent = LexDeontic.DOp => VP;
--    Simple = S;

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
    --   shall not have the effect of                -- CN N2
    --   abating, nullifying, suspending or vitiating -- VPI -> NP
    --   any public road district                     -- some_Det CN
    --   or any proceedings by any such public road district.

    -- mkCl i_NP ( mkVP ( passiveVP love_V2 ) ( mkAdv by8agent_Prep ( mkNP this_Quant ( mkCN ( mkAP young_A ) ( mkCN woman_N ) ) ) ) )

  lin    

    repealing = mkEvent
      (mkN "repealing") -- of the
      (mkNP
         (mkNP
            (mkNP and_Conj
               (mkNP the_Det (mkCN (mkN "section")))
               (mkNP aPl_Det (mkCN (mkN "law"))))
            (mkV2 "repeal") )
         ( SyntaxEng.mkAdv by8agent_Prep
             ( mkNP this_Quant (mkCN (mkN "law")))));

    -- example = mkS 
    --   ( mkCl i_NP 
    --       ( mkVP 
    --           ( mkVP love_V2 
    --               ( mkNP 
    --                   ( mkNP the_Det woman_N ) love_V2 ) ) 
    --           ( SyntaxEng.mkAdv by8agent_Prep 
    --               ( mkNP 
    --                   ( mkNP the_Det 
    --                       ( mkCN brother_N2 ) )
    --                   ( SyntaxEng.mkAdv part_Prep -- of
    --                       ( mkNP 
    --                           ( mkNP 
    --                               ( mkNP the_Det woman_N )
    --                               love_V2 ) 
    --                           ( SyntaxEng.mkAdv by8agent_Prep 
    --                               ( mkNP 
    --                                   ( mkNP the_Det 
    --                                       ( mkCN brother_N2 ) ) 
    --                                   ( SyntaxEng.mkAdv part_Prep 
    --                                       ( mkNP the_Det woman_N ) ) ) ) ) ) ) ) ) );

    abating = table {
      LexDeontic.Oblig => mkAbating some_Det;
      LexDeontic.Forb  => mkAbating  any_Det;
      LexDeontic.Perm  => mkAbating some_Det
      };

  oper
    mkAbatingShort : Det -> VP = \det ->
      mkConsequence the_Det (mkN "effect") (mkV2 "abate") det (mkN "proceeding");
    mkAbating : Det -> VP = \det ->
      mkConsequence the_Det (mkN "effect")

      (ConsVPI (MkVPI (mkVP (mkV "abate")))
      (ConsVPI (MkVPI (mkVP (mkV "nullify")))
      (BaseVPI (MkVPI (mkVP (mkV "suspend")))
               (MkVPI (mkVP (mkV "vitiate"))))))

      det (mkN "proceeding");
    
-- > l -table (ConjVPI and_Conj (ConsVPI (MkVPI (UseV sleep_V)) (BaseVPI (MkVPI (UseV sing_V)) (MkVPI (UseV eat_V)))))
-- linking ... OK

-- Languages: AllEng
-- s VVAux (AgP1 Sg) : sleep , sing and eat
-- s VVAux (AgP1 Pl) : sleep , sing and eat
-- s VVAux (AgP2 Sg) : sleep , sing and eat
-- s VVAux (AgP2 Pl) : sleep , sing and eat
-- s VVAux (AgP3Sg Neutr) : sleep , sing and eat
-- s VVAux (AgP3Sg Masc) : sleep , sing and eat
-- s VVAux (AgP3Sg Fem) : sleep , sing and eat
-- s VVAux (AgP3Pl Neutr) : sleep , sing and eat
-- s VVAux (AgP3Pl Masc) : sleep , sing and eat
-- s VVAux (AgP3Pl Fem) : sleep , sing and eat
-- s VVInf (AgP1 Sg) : to sleep , to sing and to eat
-- s VVInf (AgP1 Pl) : to sleep , to sing and to eat
-- s VVInf (AgP2 Sg) : to sleep , to sing and to eat
-- s VVInf (AgP2 Pl) : to sleep , to sing and to eat
-- s VVInf (AgP3Sg Neutr) : to sleep , to sing and to eat
-- s VVInf (AgP3Sg Masc) : to sleep , to sing and to eat
-- s VVInf (AgP3Sg Fem) : to sleep , to sing and to eat
-- s VVInf (AgP3Pl Neutr) : to sleep , to sing and to eat
-- s VVInf (AgP3Pl Masc) : to sleep , to sing and to eat
-- s VVInf (AgP3Pl Fem) : to sleep , to sing and to eat
-- s VVPresPart (AgP1 Sg) : sleeping , singing and eating
-- s VVPresPart (AgP1 Pl) : sleeping , singing and eating
-- s VVPresPart (AgP2 Sg) : sleeping , singing and eating
-- s VVPresPart (AgP2 Pl) : sleeping , singing and eating
-- s VVPresPart (AgP3Sg Neutr) : sleeping , singing and eating
-- s VVPresPart (AgP3Sg Masc) : sleeping , singing and eating
-- s VVPresPart (AgP3Sg Fem) : sleeping , singing and eating
-- s VVPresPart (AgP3Pl Neutr) : sleeping , singing and eating
-- s VVPresPart (AgP3Pl Masc) : sleeping , singing and eating
-- s VVPresPart (AgP3Pl Fem) : sleeping , singing and eating



    
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
         (mkCN (mkN2 eating) -- of
            baconAndEggs);

    mkConsequence = overload {
      mkConsequence :
        Det -> N     -> V2 -> Det -> N       -> VP =
        \the, effect,causing, some,  obesity ->
      (mkVP have_V2
         (mkNP the -- think about this as a quantifier also
            (mkCN (mkN2 effect) -- of
               (ExtensionsEng.GerundNP (mkVP causing -- mkVΠ V2
                                          (mkNP some
                                             (mkCN obesity))))))) ;

      mkConsequence : Det -> N   -> ListVPI       -> Det -> N -> VP =
                     \the,effect,causing_worsening,some,  obesity ->
      (mkVP have_V2
         (mkNP the -- think about this as a quantifier also
            (mkCN (mkN2 effect) -- of
               (ExtensionsEng.GerundNP (mkVP
                                          (listvpi2v2 causing_worsening some)
                                          (mkNP some
                                             (mkCN obesity)))))))
    };

    listvpi2v2 : ListVPI -> Det -> V2;
    listvpi2v2 listvpi det =
      let sadTable : Det => Conj = table { some_Det =>  or_Conj ; any_Det  => and_Conj };
          conj = sadTable ! det;
          vpi  = ConjVPI conj listvpi
      in  mkV2 (lessing (vpi.s ! RE.VVPresPart ! RE.AgP3Pl RE.Neutr));

    any_Det  = mkDet (ParadigmsEng.mkQuant "any"  "any");
    some_Det = mkDet (ParadigmsEng.mkQuant "some" "some");

    lessing : Str -> Str = \s -> table { x + "vitiating" => x + "vitiate"; _ => s } ! s ;

}
