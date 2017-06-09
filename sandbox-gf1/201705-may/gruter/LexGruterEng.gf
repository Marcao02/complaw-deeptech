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

    where_Subj = mkSubj "where";

}
