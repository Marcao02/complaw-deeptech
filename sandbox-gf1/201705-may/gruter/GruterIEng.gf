--# -path=.:present

concrete GruterIEng of Gruter = GruterI with
  (Syntax = SyntaxEng)
  , (LexL4     = LexL4Eng)
  , (LexGruter = LexGruterEng)
  , (LexDeontic = LexDeonticEng)
  , (LexParty   = LexPartyEng)
  , (Sentence = SentenceEng)
  ;
