incomplete concrete BaseI of Base = open Syntax, LexSize in {
  lincat
    FoodItem = N;
    Mod = A;
    Clause = CN;
  lin
    Statement m p = mkCN m p;
    BaseSmall  = sml2A Size_S;
    BaseMedium = sml2A Size_M;
    BaseLarge  = sml2A Size_L;
    Potato = potato;
}
      