--# -path=.:present

abstract Missouri =
  Gruter **
  {
  flags startcat = Contract;

  cat
    ReactionRule;
    Event;
    Consequent Event;
        
  fun
    Missouri_When  : WhenPredicate;
    Missouri_Party : Party;
    Win_Kind     : ActionKind;
    Win_Act      : Action Win_Kind;
    Win_Exp      : ActionExp Win_Kind;

    eating       : Event;
    obesity      : Consequent eating;

--     repealing    : Event;
--     abating      : Consequent repealing;
    
    mkReactionRule :
         (event : Event)
      -> Deontic
      -> Consequent event
      -> ReactionRule;
}
      