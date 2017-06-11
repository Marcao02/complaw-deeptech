incomplete concrete LondonI of London =
  GruterI **
  open Syntax, Sentence, LexGruter, LexDeontic, LexParty in
  {
  oper
    LPP : P_Party = {
       prenom  = "Winston";
       surname = "Churchill";
       order   = FnFirst };
  lin
    London_Party = partyname LPP;
}
                                     