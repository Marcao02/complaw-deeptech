
-- functor for Gruter L4

incomplete concrete GruterI of Gruter =
  open Syntax, ParadigmsEng, ExtraEng
  in {
  lincat
    Contract = Utt;
    WhenPredicate = Adv;
    Party = PN;
    Deontic = VV;
    Action = VP;
    ActionExp = Cl;
  lin
    Clause when party deon act actexp =
      mkUtt (mkS
               (mkTemp presentTense simultaneousAnt)
                                     deon.pol
                                     (mkCl party deon.vv act));

    Deliver_Act = mkVP P_deliver_V2 (mkNP (P_things_N));
    Deliver_Exp = mkCl (mkNP P_things_N) (mkAP P_correct_A);

    Party_A = partyname P_Party_A;
    Party_B = partyname P_Party_B;
    Party_C = partyname P_Party_C;

    Pay_Act = mkVP (mkVV "pay");
    Pay_Exp = mkCl (mkNP P_pay_N) P_correct_A;

    P_deliver_V2 = mkV2 "deliver";
    P_things_N   = mkN "things";
    P_correct_A  = mkA "correct";
    P_pay_N      = mkN "payment";


    {- PARTY LOGIC -}
  param
    NameOrder = FnFirst | SnFirst;

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

    partyname : P_Party -> Party;
    partyname p = mkPN (case p.order of {
                          FnFirst => (p.prenom ++ p.surname )
                            ; SnFirst => (p.surname ++ p.prenom )
                          });


        
    {- DEONTIC LOGIC -}
  lin
    
    Shall   = { d = Oblig; pol = positivePol; vv = must_VV };
    MustNot = { d = Forb;  pol = negativePol; vv = must_VV };
    May     = { d = Perm;  pol = positivePol; vv =  may_VV };

  oper
    Deon : Type = { d   : DOp
                  ; pol : Pol
                  ; vv  : VV
    };
  param
    DOp = Oblig | Forb | Perm ;
               
}
