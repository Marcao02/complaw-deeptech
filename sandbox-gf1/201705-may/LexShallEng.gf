instance LexShallEng of LexShall = open SyntaxEng, ParadigmsEng, IrregEng, ExtraEng in {
  oper
    may = may_VV;
    pay_V = mkV "pay";
    shipGoods_VP = mkVP (mkV2 "ship") (mkNP (mkN "goods" "goods"));

    partyname p = mkPN (case p.order of {
                             FnFirst => (p.prenom ++ p.surname )
                           ; SnFirst => (p.surname ++ p.prenom )
                            });
}

