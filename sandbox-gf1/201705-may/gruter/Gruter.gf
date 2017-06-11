
-- a simple L4 implementation.

abstract Gruter = {
  flags startcat = Contract ;
  cat
    Contract;

    -- predicate logic
    WhenPredicate;

    -- logic of entities
    Party;

    -- logic of obligation, permission, and prohibition
    Deontic;

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

    Default_When                : WhenPredicate;
    Shall, MustNot, May         : Deontic;
    Default_Party               : Party;
    Default_ActionKind          : ActionKind;
    Default_Act                 : Action    Default_ActionKind;
    Default_Exp                 : ActionExp Default_ActionKind;
--    Before_T                          : Temporal_Deadline;
 }
 
