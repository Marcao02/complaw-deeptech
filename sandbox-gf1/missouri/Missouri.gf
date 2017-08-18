--# -path=.:present:/Users/mengwong/non-db-src/l/compiler/sandbox-gf1/201705-may/gruter

abstract Missouri =
  Gruter **
  {
  flags startcat = Contract;
  fun
    Missouri_When  : WhenPredicate;
    Missouri_Party : Party;
    Win_Kind     : ActionKind;
    Win_Act      : Action Win_Kind;
    Win_Exp      : ActionExp Win_Kind;
}
      