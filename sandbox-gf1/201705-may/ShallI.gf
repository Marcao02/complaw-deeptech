incomplete concrete ShallI of Shall =
    open Syntax, LexShall in {
  lincat
    Contract = Utt ;
    Party = NP ;
    Deontic = VV ;
    Action = VP;
  lin
    Pred alice must pay = mkUtt (mkCl alice must pay);

    Shall   = shall_VV;
    MustNot = mustNot_VV; -- switch this to use Polarity
    May     = may_VV;

    Alice   = mkNP Alice_PN;
    Bob     = mkNP Bob_PN;
    Carol   = mkNP Carol_PN;
    
    Pay     = mkVP pay_V;
    ShipGoods = mkVP ship_V2 (mkNP goods_N);
}

    
