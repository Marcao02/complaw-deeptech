instance LexGruterEng of LexGruter =
  open SyntaxEng, ParadigmsEng, IrregEng, ExtraEng
  in {
  oper
    
    P_deliver_V2 = mkV2 "deliver";
    P_things_NP  = mkNP the_Quant pluralNum (mkN "thing");
    P_correct_A  = mkA "correct";
    P_hot_A      = mkA "hot";
    P_pay_N      = mkN "payment";

    P_pay_V      = mkV "pay";

    P_the_sun = mkNP the_Det (mkN "sun");
    P_shines  = mkA "shining";
    P_blue    = mkA "blue";
    P_the_moon = mkNP the_Det (mkN "moon");

    P_default_act = mkVP (mkV2 "perform") (mkNP a_Det (mkN "action"));
    P_default_exp = mkCl (mkNP the_Det (mkN "action")) P_correct_A;
    
    -- https://groups.google.com/forum/#!topic/gf-dev/O4z1lh2u0v4
    P_by_default = SyntaxEng.mkAdv by8means_Prep (mkNP (mkN "default"));
    
    where_Subj = mkSubj "where";
}
