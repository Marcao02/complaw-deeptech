abstract Base = {
  flags startcat = Clause;
  cat
    FoodItem;
    Mod;
    Clause;
  fun
    Statement : Mod -> FoodItem -> Clause;
    BaseSmall, BaseMedium, BaseLarge : Mod;
    Potato : FoodItem;
}
      