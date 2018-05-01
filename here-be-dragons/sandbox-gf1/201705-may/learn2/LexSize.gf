interface LexSize = open Syntax, Constructors, Structural in {
  param
    SML = Small | Medium | Large;
    
  oper
    SizeType : Type = { size : SML };
    Size_S, Size_M, Size_L : SizeType;
    sml2A : SizeType -> A;
  oper
    Size_S = { size = Small };
    Size_M = { size = Medium };
    Size_L = { size = Large };
}
      