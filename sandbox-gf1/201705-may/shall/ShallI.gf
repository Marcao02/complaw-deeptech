incomplete concrete ShallI of Shall =
    open Syntax, LexShall in {
  lincat
    Contract = Utt ;
    Party = NP ;
    Deontic = Deon ;
    Action = VP;
  lin
    Pred alice deon pay = mkUtt (mkS (mkTemp presentTense simultaneousAnt)
                                     deon.pol
                                     (mkCl alice deon.vv pay));

    Shall   = { d = Oblig; pol = positivePol; vv = must_VV };
    May     = { d = Perm; pol = positivePol; vv =  may    };
    MustNot = { d = Forb; pol = negativePol; vv = must_VV };

    Alice   = mkNP (partyname Alice_P);
    Bob     = mkNP (partyname Bob_P);
    Carol   = mkNP (partyname Carol_P);
    
    Pay     = mkVP pay_V;
    ShipGoods = shipGoods_VP;
  oper
    -- decompose logic properly to have a Bool and a Deon
    -- the Bool should also work for other logics like Temporal
    Deon : Type = { d   : DOp
                  ; pol : Pol
                  ; vv  : VV
    };
  param
    DOp = Oblig | Forb | Perm ;
}

    
