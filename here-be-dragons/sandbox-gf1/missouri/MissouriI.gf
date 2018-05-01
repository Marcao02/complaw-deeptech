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

    abate = table {
      LexDeontic.Oblig => mkAbating some_Det;
      LexDeontic.Forb  => mkAbating  any_Det;
      LexDeontic.Perm  => mkAbating some_Det
      };

  oper

    mkAbating : Det -> VP = \det ->
      mkConsequence the_Det (mkN "effect")

      (ConsVPI (MkVPI (mkVP (mkV "abate")))
      (ConsVPI (MkVPI (mkVP (mkV "nullify")))
      (BaseVPI (MkVPI (mkVP (mkV "suspend")))
               (MkVPI (mkVP (mkV "vitiate"))))))

      any_Predet
      (mkNP
         or_Conj

         (mkNP 
            (mkNP 
               (mkNP (mkCN (mkN "public road district")))
               (mkV2 (mkV "incorporate"))
            )
            ( SyntaxEng.mkAdv
                prior_to_Prep
                (mkNP the_Det
                   (mkCN (mkN "taking-effect of this law")))))

         (mkNP any_Predet
            (mkNP (mkCN
               (mkN2
                  (mkN "proceeding")
                  by8agent_Prep)
               (mkNP any_Predet
                  (mkNP such_Predet
                     (mkNP (mkCN (mkN "public road district"))))))))
         );


    
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
         (mkNP the -- think about this as a quantifier
            (mkCN (mkN2 effect) -- of
               (ExtensionsEng.GerundNP (mkVP causing -- mkVΠ V2
                                          (mkNP some
                                             (mkCN obesity))))))) ;

      mkConsequence : Det -> N   -> ListVPI     -> SS -> NP -> VP =
                     \the,effect,causing_worsening,some,  obesity ->
      (mkVP have_V2
         (mkNP the -- think about this as a quantifier
            (mkCN (mkN2 effect) -- of
               (ExtensionsEng.GerundNP (mkVP
                                          (listvpi2v2 causing_worsening some)
                                          (mkNP some obesity))))))
    };

    listvpi2v2 : ListVPI -> SS -> V2;
    listvpi2v2 listvpi predet =
      let sadTable : SS => Conj = table { some_Predet =>  or_Conj ; any_Predet  => and_Conj };
          conj = sadTable ! predet;
          vpi  = ConjVPI conj listvpi
      in  mkV2 (lessening (vpi.s ! RE.VVPresPart ! RE.AgP3Pl RE.Neutr));

    any_Det  = mkDet (ParadigmsEng.mkQuant "any"  "any");
    some_Det = mkDet (ParadigmsEng.mkQuant "some" "some");

    
    any_Predet  = lin Predet (ss "any");
    some_Predet = lin Predet (ss "some");
    all_Predet  = lin Predet (ss "all");
    such_Predet = lin Predet (ss "such");

    
    lessening : Str -> Str = \s -> table { x + "vitiating" => x + "vitiate"; _ => s } ! s ;

    prior_to_Prep : Prep = mkPrep "prior to";

}
