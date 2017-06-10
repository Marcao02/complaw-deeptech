instance LexBaseEng of LexBase =
  open SyntaxEng, ParadigmsEng in {
  oper
    small  = mkA "small";
    large  = mkA ( variants { "large" ; "big" } );
    potato = mkN "potato";
}
      