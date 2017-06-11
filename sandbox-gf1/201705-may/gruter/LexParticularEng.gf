instance LexParticularEng of LexParticular =
  LexGruterEng - [P_desirable
                    ,P_by_default
                    ,P_default_act
                    ,P_default_exp
                    ] **
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

    -- https://groups.google.com/forum/#!topic/gf-dev/O4z1lh2u0v4
    P_always = SyntaxEng.mkAdv   if_Subj (mkS (mkCl P_the_sun  P_shines));


    P_deliver_exp = mkCl P_things_NP (mkAP P_hot_A);
    P_pay_exp = mkCl (mkNP the_Det P_pay_N) P_correct_A;
}
      