 incomplete concrete PhoenixI of Phoenix =
    open Syntax, LexPhoenix, ResParty in {
  lincat
    Contract = Utt;
    WhenPredicate = Cl;
    Party = PN;
    Deontic = Deon;
    Action = VP;
    ActionExp = Cl;
--    Temporal_Deadline = Adv;

 -- move this to a separate deontic logic module
  oper
    Deon : Type = { d   : DOp
                  ; pol : Pol
                  ; vv  : VV
    };
  param
    DOp = Oblig | Forb | Perm ;


  lin
{-
    Clause when party deontic action actionexp
      = mkUtt (mkS (mkTemp presentTense simultaneousAnt)
                    deontic.pol
                    (mkCl (mkNP party) deontic.vv action));
-}

    Clause when party deontic action actionexp
      = mkUtt (mkCl (mkNP party) action);

    Deliver_Act = mkVP P_deliver_V2 (mkNP (P_things_N));
    Deliver_Exp = mkCl (mkNP P_things_N) (mkAP P_correct_A);
    Shall   = { d = Oblig; pol = positivePol; vv = must_VV };
    May     = { d = Perm; pol = positivePol; vv =  may    };
    MustNot = { d = Forb; pol = negativePol; vv = must_VV };

    Party_A = partyname P_Party_A;
    Party_B = partyname P_Party_B;
    Party_C = partyname P_Party_C;

    Pay_Act = mkVP pay_V;
    Pay_Exp = mkCl (mkNP P_pay_N) P_correct_A;

}


