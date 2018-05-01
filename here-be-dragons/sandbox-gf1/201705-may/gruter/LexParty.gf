
interface LexParty = open Syntax in {
  param
    NameOrder = FnFirst | SnFirst;
  oper
    P_Party : Type = { prenom  : Str
                     ; surname : Str
                     ; order   : NameOrder
    };

    partyname,
    defaultpartyname : P_Party -> NP;
    defaultpartyname p = mkNP (mkPN (case p.order of {
                                FnFirst => (p.prenom ++ p.surname ) ;
                                SnFirst => (p.surname ++ p.prenom )
                                }));
}
      