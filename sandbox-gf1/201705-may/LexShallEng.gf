instance LexShallEng of LexShall = open SyntaxEng, ParadigmsEng, IrregEng in {
  oper
    shall_VV = mkVV (mkV "shall");
    mustNot_VV = mkVV (mkV "shall not"); -- TODO: use Polarity
    may_VV = mkVV (mkV "may");

    Alice_PN = mkPN "Alice";
    Bob_PN   = mkPN "Bob";
    Carol_PN = mkPN "Carol";

    pay_V = mkV "pay";
    ship_V2 = mkV2 "ship";
    goods_N = mkN "goods" "goods";
}

