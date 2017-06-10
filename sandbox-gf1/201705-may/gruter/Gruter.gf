
-- a simple L4 implementation.

abstract Gruter = L4 ** {
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

    Always, BlueMoon            : WhenPredicate;
    Shall, MustNot, May         : Deontic;
    Party_A, Party_B, Party_C   : Party;
    Pay_Kind, Deliver_Kind      : ActionKind;
    Pay_Act                     : Action Pay_Kind;
    Deliver_Act                 : Action Deliver_Kind;
    Pay_Exp                     : ActionExp Pay_Kind;
    Deliver_Exp                 : ActionExp Deliver_Kind;
--    Before_T                          : Temporal_Deadline;
 }
 
