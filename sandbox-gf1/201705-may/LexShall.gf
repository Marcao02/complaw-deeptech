interface LexShall = open Syntax in {
  oper
    Party : Type = { prenom  : Str
                   ; surname : Str
                   ; order   : NameOrder
    };
    partyname   : Party -> PN;

    Alice_PN = partyname Alice;
    Bob_PN   = partyname Bob;
    Carol_PN = partyname Carol;

    Alice = { prenom="Alice" ; surname="Angle"        ; order = FnFirst }; 
    Bob   = { prenom="Bill"  ; surname="Baobean"      ; order = SnFirst }; 
    Carol = { prenom="Carol" ; surname="Cryptologist" ; order = FnFirst }; 

  param
    NameOrder = FnFirst | SnFirst;
}
