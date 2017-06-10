abstract Base = {
  flags startcat = Clause;
  cat
    FoodItem;
    Mod;
    Clause;
  fun
    Statement : Mod -> FoodItem -> Clause;
    Small : Mod;
    Large : Mod;
    Potato : FoodItem;
}
      