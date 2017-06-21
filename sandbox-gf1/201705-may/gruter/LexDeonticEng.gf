instance LexDeonticEng of LexDeontic =
  open SyntaxEng, ExtraEng
  in {
  oper
    Deontic_must_VV = must_VV | shall_VV ;
    Deontic_may_VV  = may_VV;
}
