--based on https://github.com/legalese/legalese-compiler/blob/56c97731d282ad914a7218b64d1dc07525cdf772/linear_state_machine_language/examples_to_port_to_pyL4/examplesLSM4/monster_burger_lsm_nlg.md

abstract MonsterBurgerv2 = {

  flags startcat = Challenge ;

  cat
    Challenge ;
    Party1 ;
    Party2 ;
    Object1 ;
    Object2 ;
    Object3 ;
    Object3Definition ;
    Object4 ;
    Object4Definition ;
    Object4Definition2 ;
    Object5 ;
    Object5Definition ;
    Object6 ;
    Object7 ;
    Object8 ;
    Object9 ;
    Object10 ;
    Deontic1 ;
    Deontic2 ;
    Deontic3 ;
    Deontic4 ;
    Deontic5 ;
    Deontic6 ;
    Party1Action1 ;
    Party1Action2 ;
    Party1Action3 ;
    Party2Action1 ;
    Party2Action2 ;
    Party2Action3 ;
    Party2Action4 ;
    Party2Action5 ;
    Party2Action6 ;
    Party2Action7 ;
    Party2Action8 ;
    TimeExpression1 ;
    TimeExpression2 ;
    TimeExpression3 ;
    TimeExpression4 ;
    TimeExpression5 ;
    TimeExpression6 ;
    TimeExpression7 ;
    TimeExpression8 ;
    TimeExpression9 ;
    TimeExpression10 ;
    TimeExpression11 ;
    ControlFlow1 ;
    ControlFlow2 ;
    ControlFlow3 ;
    ControlFlow4 ;
    ControlFlow5 ;
    

  fun
--sentence constructions
--sentence 1
    aTransition : TimeExpression1 -> Challenge ;
    bObligation : Party2 -> Deontic1 -> Object1 -> TimeExpression2 -> Challenge ;
    cViolation : ControlFlow1 -> Party2 -> Party2Action1 -> Object2 -> Challenge ;
--sentence 2    
    dTransition : TimeExpression3 -> Challenge ;
    eTerm : Object3 -> Object3Definition -> Challenge ;
    fObligation : Object4 -> Object4Definition -> Challenge ;
    gTerm : Object5 -> Object5Definition -> Challenge ;
--sentence 3
    hTransition : TimeExpression4 -> Object5 -> Object5Definition -> Challenge ;
    iPermission : Party1 -> Deontic2 -> Object1 -> TimeExpression5 -> Challenge ;
    jAlternative : ControlFlow1 -> Object5 -> Party2Action3 -> Challenge ;
    kAlternativeCondition : ControlFlow2 -> Object6 -> Party2Action4 -> Object3 -> Challenge ;
--sentence 4    
    lTransition : TimeExpression9 -> Challenge ;
    mObligation : Party2 -> Deontic3 -> Party1 -> TimeExpression6 -> Challenge ;
    nViolation : ControlFlow1 -> Party2 -> Party2Action1 -> Object7 -> Challenge ;
--sentence 5    
    oCondition : ControlFlow2 -> Party2 -> Party2Action1 -> Object7 -> Challenge ;
    pCondition : ControlFlow3 -> Party2 -> Party2Action1 -> Object2 -> Challenge ;
    qCondition : ControlFlow3 -> Party2 -> Party2Action5 -> Party1 -> Party1Action2 -> Challenge ;
    rConditionTrue : ControlFlow4 -> Object4 -> Object4Definition2 -> Challenge ;
    sConditionTrue : ControlFlow5 -> Object8 -> Party2Action6 -> Challenge ;
--sentence 6
    tTransition : TimeExpression10 -> Party2 -> Party2Action7 -> Party1 -> Party1Action2 -> Challenge ;
    uPermission : Party2 -> Deontic4 -> TimeExpression7 -> Challenge ;
    vPermission : Party2 -> Deontic5 -> TimeExpression7 -> Challenge ;
--sentence 7
    wTransition : TimeExpression11 -> Challenge ;
    xObligation : ControlFlow4 -> Party1 -> Deontic6 -> Object9 -> Challenge ;
    yObligationCondition : ControlFlow2 -> Object6 -> Party2Action4 -> Object3 -> Challenge ;
    zAlternative : ControlFlow1 -> Object5 -> Object5Definition -> Challenge ;
    zaAlternativeCondition : ControlFlow2 -> Object6 -> TimeExpression8 -> Object3 -> Challenge ;
--sentence 8
    zbCondition : ControlFlow2 -> Party1 -> Party1Action3 -> Object9 -> Challenge ;
    zcConditionTrue : ControlFlow4 -> Object10 -> Object4 -> Challenge ;
    zdConditionTrue : ControlFlow5 -> Object4 -> Object4Definition2 -> Challenge ;
    zeConditionTrue : ControlFlow5 -> Object8 -> Party2Action6 -> Challenge ;   
    --noun phrases, verb phrases, adverbial phrases. Prepositional phrases (thanks Inari :) for the Upon parameters!) 
    Customer : Party1 ;
    Restaurant : Party2 ;
    MonsterBurger : Object1 ;
    PromptServeGuarantee : Object2 ;
    ChallengeEndTime : Object3 ;
    CurrentTimePlus1 : Object3Definition ;
    AmountOwing : Object4 ;
    FiftyDollars : Object4Definition ;
    ZeroDollar : Object4Definition2 ;
    TheChallenge : Object5 ;
    Continue : Object5Definition ;
    Time : Object6 ;
    PromptCheckGuarantee : Object7 ;
    Contract : Object8 ;
    Bill : Object9 ;
    AmountPaid : Object10 ;
    ShouldServe : Deontic1 ;
    MayFinish : Deontic2 ;
    ShouldCheck : Deontic3 ;
    MayConfirm : Deontic4 ;
    MayDisconfirm : Deontic5 ;
    MustPay : Deontic6 ;
    Order : Party1Action1 ;
    Finish : Party1Action2 ;
    Pay : Party1Action3 ;
    Violate : Party2Action1 ;
    Serve : Party2Action2 ;
    Over : Party2Action3 ;
    Reach : Party2Action4 ;
    Confirm : Party2Action5 ;
    Fulfill : Party2Action6 ;
    Check : Party2Action7 ;
    Disconfirm : Party2Action8 ;
    Upon1 : Party1 -> Party1Action1 -> Object1 -> TimeExpression1 ;
    Within15minutes : TimeExpression2 ;
    Upon2 : Party2 -> Party2Action2 -> Object1 -> TimeExpression3 ;
    Whenever : TimeExpression4 ;
    BeforeChallengeEndTime : TimeExpression5 ;
    Within10minutes : TimeExpression6 ;
    Immediately : TimeExpression7 ;
    Before : TimeExpression8 ;
    Upon3 : TimeExpression9 ;
    Upon4 : TimeExpression10 ;
    Upon5 : Party2 -> Party2Action8 -> Party1 -> TimeExpression11 ;
    Otherwise : ControlFlow1 ;
    If : ControlFlow2 ;
    Or : ControlFlow3 ;
    Then : ControlFlow4 ;
    And : ControlFlow5 ;
}