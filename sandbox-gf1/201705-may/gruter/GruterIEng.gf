--# -path=.:present

concrete GruterIEng of Gruter = DeonticI ** GruterI with
  (Syntax = SyntaxEng)
  , (LexGruter = LexGruterEng)
  , (LexDeontic = LexDeonticEng)
  , (LexParty   = LexPartyEng)
  , (Sentence = SentenceEng)
  ;
