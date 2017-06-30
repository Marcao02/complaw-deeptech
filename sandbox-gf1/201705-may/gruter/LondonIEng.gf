--# -path=.:present

concrete LondonIEng of London = GruterIEng **
  LondonI
  with
  (Syntax     =     SyntaxEng),
  (Sentence   =   SentenceEng),
  (LexDeontic = LexDeonticEng),
  (LexParty   =   LexPartyEng),
  (LexGruter  =  LexGruterEng),
  (LexLondon  =  LexLondonEng)
  ;

