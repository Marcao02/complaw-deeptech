abstract London =
  Gruter **
  {
  flags startcat = Contract;
  fun
    London_When  : WhenPredicate;
    London_Party : Party;
    Win_Kind     : ActionKind;
    Win_Act      : Action Win_Kind;
    Win_Exp      : ActionExp Win_Kind;
}
      