instance LexGruterEng of LexGruter =
  open SyntaxEng, ParadigmsEng, IrregEng, ExtraEng
  in {
  oper
    
    P_deliver_V2 = mkV2 "deliver";
    P_things_N   = mkN "things";
    P_correct_A  = mkA "correct";
    P_pay_N      = mkN "payment";

    easypay = may_VV;

    P_pay_V      = mkV "pay";

    P_the_sun = mkNP the_Det (mkN "sun");
    P_shines  = mkV "shine";
    P_blue    = mkA "blue";
    P_the_moon = mkNP the_Det (mkN "moon");

}
