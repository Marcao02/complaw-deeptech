--# -path=.:present:/Users/mengwong/non-db-src/l/compiler/sandbox-gf1/201705-may/gruter

incomplete concrete MissouriI of Missouri =
  GruterI **
  {
  oper
    LPP : P_Party = {
       prenom  = "Winston";
       surname = "Churchill";
       order   = FnFirst };
  lin
    Missouri_Party = partyname LPP;
    -- ontology of when-predicate conditions
    Missouri_When   = P_by_default;
    Win_Kind      = <>;
    Win_Act       = mkVP L_win_V;
    Win_Exp       = L_win_exp;
}
                                     