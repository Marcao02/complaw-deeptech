instance LexDeonticEng of LexDeontic =
  open SyntaxEng, ExtraEng
  in {
  oper
    Deontic_must_VV = shall_VV | must_VV;
    Deontic_may_VV  = may_VV;
}
