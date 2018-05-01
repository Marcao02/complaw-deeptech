--# -path=.:present

concrete BaseIEng of Base = BaseI with
  (Syntax = SyntaxEng),
  (Sentence = SentenceEng),
  (LexBase = LexBaseEng),
  (LexSize = LexSizeEng)
  ;
