--# -path=.:present

concrete ParticularIEng of Particular = GruterIEng
  - [Default_Party,
                                                             Default_ActionKind,
                                                             Default_Act,
                                                             Default_Exp,
                                                             Default_When]
  **
  ParticularI with
  (Syntax = SyntaxEng)
  , (Sentence = SentenceEng)
  , (LexDeontic = LexDeonticEng)
  , (LexParty   = LexPartyEng)
  , (LexParticular = LexParticularEng)
  ;
