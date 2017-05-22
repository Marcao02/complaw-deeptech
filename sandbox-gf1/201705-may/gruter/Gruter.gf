
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
    Action; ActionExp;

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
    Clause                      : WhenPredicate
                               -> Party
                               -> Deontic
                               -> Action
                               -> ActionExp
                               -> Contract;
    Always, BlueMoon            : WhenPredicate;
    Shall, MustNot, May         : Deontic;
    Party_A, Party_B, Party_C   : Party;
    Pay_Act, Deliver_Act        : Action;
    Pay_Exp, Deliver_Exp        : ActionExp;
--    Before_T                          : Temporal_Deadline;
 }
 
