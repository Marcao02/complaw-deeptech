
-- a simple L4 implementation.

abstract Gruter = Deontic, Party ** {
  flags startcat = Contract ;
  cat
    Contract;

    -- predicate logic
    WhenPredicate;

    -- domain-specific action expressions
    ActionKind; -- dependently typed
    Action ActionKind; ActionExp ActionKind;

 {- todo
    -- temporal logic
    Temporal_Deadline;

    -- rule logic
    Rule_SubjectTo; Rule_Notwithstanding; Rule_Provided;

    -- dynamic logic
    mkValid; rmValid;

    -- control flow
    Connective_Hence; Connective_Lest;

    -- epistemic logic
    Notify; Learn; Declare; Publish;
-}

  fun
    Clause :
         WhenPredicate
      -> Party
      -> Deontic
      -> (k : ActionKind)
      -> Action k
      -> ActionExp k
      -> Contract;

--    Before_T                          : Temporal_Deadline;
 }
 
