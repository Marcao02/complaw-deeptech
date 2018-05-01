instance LexDeonticIta of LexDeontic =
  open SyntaxIta, ExtraIta
  in {
  oper
    Deontic_must_VV = must_VV | shall_VV ;
    Deontic_may_VV  = may_VV;
}
