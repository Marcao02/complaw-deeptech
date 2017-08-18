--# -path=.:present

abstract Missouri =
  Gruter **
  {
  flags startcat = Contract;

  cat
    ReactiveRule;
    Happening;
        
  fun
    Missouri_When  : WhenPredicate;
    Missouri_Party : Party;
    Win_Kind     : ActionKind;
    Win_Act      : Action Win_Kind;
    Win_Exp      : ActionExp Win_Kind;

    eating       : Happening;
    
    mkReactiveRule :
         Happening
      -> Deontic
      -> Happening
      -> ReactiveRule;
}
      