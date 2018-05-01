incomplete concrete LondonI of London =
  GruterI **
  {
  oper
    LPP : P_Party = {
       prenom  = "Winston";
       surname = "Churchill";
       order   = FnFirst };
  lin
    London_Party = partyname LPP;
    -- ontology of when-predicate conditions
    London_When   = P_by_default;
    Win_Kind      = <>;
    Win_Act       = mkVP L_win_V;
    Win_Exp       = L_win_exp;
}
                                     