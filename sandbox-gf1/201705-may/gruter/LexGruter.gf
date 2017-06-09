interface LexGruter = open Syntax in {
  oper
    P_Party : Type = { prenom  : Str
                   ; surname : Str
                   ; order   : NameOrder
    };

    P_Party_A : P_Party
            = { prenom="Alice"
              ; surname="Andromeda"
              ; order = FnFirst }; 
    P_Party_B : P_Party
            = { prenom="Bobo"
              ; surname="Bai"
              ; order = SnFirst }; 
    P_Party_C : P_Party
            = { prenom="Carol"
              ; surname="Centaurus"
              ; order = FnFirst }; 

    partyname : P_Party -> NP;
    partyname p = mkNP (mkPN (case p.order of {
                                FnFirst => (p.prenom ++ p.surname ) ;
                                SnFirst => (p.surname ++ p.prenom )
                                }));

    P_the_sun   : NP;
    P_the_moon  : NP;
    P_blue      : A;
    P_hot_A     : A;
    P_correct_A : A;
    P_things_NP : NP;

    where_Subj  : Subj;
    
  param
    NameOrder = FnFirst | SnFirst;


    
}
