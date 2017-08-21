--# -path=.:present

abstract Missouri =
  Gruter **
  {
  flags startcat = Contract;

  cat
    ReactionRule;
    Event;
    Consequent Event;
--    Simple;
        
  fun
    Missouri_When  : WhenPredicate;
    Missouri_Party : Party;
    Win_Kind     : ActionKind;
    Win_Act      : Action Win_Kind;
    Win_Exp      : ActionExp Win_Kind;

    eating       : Event;
    obesity      : Consequent eating;

    repealing    : Event;
    abating      : Consequent repealing;

--    example      : Simple;
    
    mkReactionRule :
         (event : Event)
      -> Deontic
      -> Consequent event
      -> ReactionRule;
}
      