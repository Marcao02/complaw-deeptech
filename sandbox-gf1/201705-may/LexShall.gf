interface LexShall = open Syntax in {
  oper
    Party : Type = { prenom  : Str
                   ; surname : Str
                   ; order   : NameOrder
    };
    partyname   : Party -> PN;

    Alice_P : Party = { prenom="Alice" ; surname="Angle"        ; order = FnFirst }; 
    Bob_P   : Party = { prenom="Bill"  ; surname="Baobean"      ; order = SnFirst }; 
    Carol_P : Party = { prenom="Carol" ; surname="Cryptologist" ; order = FnFirst }; 

  param
    NameOrder = FnFirst | SnFirst;
}
