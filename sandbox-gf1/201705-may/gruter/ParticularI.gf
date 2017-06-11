
-- functor for Particular

incomplete concrete ParticularI of Particular = GruterI - [Default_Party,
                                                             Default_ActionKind,
                                                             Default_Act,
                                                             Default_Exp,
                                                             Default_When]
  **
  {
  oper
    -- PARTY CONFIGURATION
    P_Party_A : P_Party = { prenom="Alice" ; surname="Andromeda" ; order = FnFirst }; 
    P_Party_B : P_Party = { prenom="Bobo"  ; surname="Bai"       ; order = SnFirst }; 
    P_Party_C : P_Party = { prenom="Carol" ; surname="Centaurus" ; order = FnFirst }; 

  lin
    -- PARTY LOGIC -- see LexParty
    Party_A = partyname P_Party_A;
    Party_B = partyname P_Party_B;
    Party_C = partyname P_Party_C;
  
    -- ontology of actions and action-expressions
    Deliver_Act = mkVP P_deliver_V2 P_things_NP;
    Deliver_Exp = P_deliver_exp;

    Pay_Act = mkVP P_pay_V;
    Pay_Exp = P_pay_exp;

    -- these are only used for disambiguating dependent types
    Pay_Kind, Deliver_Kind = <>;

    Always   = P_always;
    BlueMoon = mkAdv when_Subj (mkS (mkCl P_the_moon P_blue));
}
  
                                                            