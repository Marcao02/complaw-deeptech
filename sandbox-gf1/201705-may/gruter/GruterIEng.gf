--# -path=.:present

concrete GruterIEng of Gruter = GruterI with
  (Syntax = SyntaxEng)
  , (LexGruter = LexGruterEng)
  , (LexDeontic = LexDeonticEng)
  , (LexParty   = LexPartyEng)
  , (Sentence = SentenceEng)
  ;
