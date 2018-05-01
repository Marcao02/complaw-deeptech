
-- a simple L4 implementation. we should also try doing this as a BNF.

abstract Phoenix = {
  flags startcat = Contract ;
  cat
    Contract;

    -- prepositional logic
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

    -- logic of rules
    Rule_SubjectTo; Rule_Notwithstanding; Rule_Provided;

    -- dynamic logic
    mkValid; rmValid;

    -- control flow
    Connective_Hence; Connective_Lest;

    -- epistemic logic
    Notify; Learn; Declare; Publish;
-}

  fun
    Clause                            : WhenPredicate
                                     -> Party
                                     -> Deontic
                                     -> Action
                                     -> ActionExp
--                                     -> Temporal_Deadline
                                     -> Contract;
    Shall, MustNot, May               : Deontic;
    Party_A, Party_B, Party_C         : Party;
    Pay_Act, Deliver_Act              : Action;
    Pay_Exp, Deliver_Exp              : ActionExp;
--    Before_T                          : Temporal_Deadline;
 }
 
