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

    easypay : VV;

    partyname : P_Party -> NP;
    partyname p = mkNP (mkPN (case p.order of {
                                FnFirst => (p.prenom ++ p.surname ) ;
                                SnFirst => (p.surname ++ p.prenom )
                                }));

    P_the_sun  : NP;
    P_shines   : V;
    P_the_moon : NP;
    P_blue     : A;
    
  param
    NameOrder = FnFirst | SnFirst;

    {- DEONTIC LOGIC -}
  oper
    
    P_Shall   : Deon = { d = Oblig; pol = positivePol; vv = must_VV };
    P_MustNot : Deon = { d = Forb;  pol = negativePol; vv = must_VV };
    P_May     : Deon = { d = Perm;  pol = positivePol; vv =  easypay };

  oper
    Deon : Type = { d   : DOp
                  ; pol : Pol
                  ; vv  : VV
    };
  param
    DOp = Oblig | Forb | Perm ;

    
}
