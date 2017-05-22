-- resource defining the party particulars.
-- in production, the runtime host will define these particulars.

resource ResParty = open Syntax in {
  param
    NameOrder = FnFirst | SnFirst;

  oper
    Party : Type = { prenom  : Str
                   ; surname : Str
                   ; order   : NameOrder
    };

  oper
    P_Party_A : Party
            = { prenom="Alice"
              ; surname="Andromeda"
              ; order = FnFirst }; 
    P_Party_B : Party
            = { prenom="Bobo"
              ; surname="Bai"
              ; order = SnFirst }; 
    P_Party_C : Party
            = { prenom="Carol"
              ; surname="Centaurus"
              ; order = FnFirst }; 

}

