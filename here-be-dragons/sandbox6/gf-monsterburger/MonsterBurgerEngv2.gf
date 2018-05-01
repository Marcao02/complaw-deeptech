--based on https://github.com/legalese/complaw-deeptech/blob/56c97731d282ad914a7218b64d1dc07525cdf772/L4/_clean_me/examples_to_port_to_pyL4/examplesLSM4/monster_burger_lsm_nlg.md

concrete MonsterBurgerEngv2 of MonsterBurgerv2 =
  open SyntaxEng, ParadigmsEng, (E=ExtendEng), (S=SentenceEng) in {

    lincat
      Challenge = Utt ;
      Party1 = NP ;
      Party2 = NP ;
      Object1 = NP ;
      Object2 = NP ;
      Object3 = NP ;
      Object3Definition = VP ;
      Object4 = NP ;
      Object4Definition = VP ;
      Object4Definition2 = VP ;
      Object5 = NP ;
      Object5Definition = VP ;
      Object6 = NP ;
      Object7 = NP ;
      Object8 = NP ;
      Object9 = NP ;
      Object10 = NP ;
      Deontic1 = V2 ;
      Deontic2 = V2 ;
      Deontic3 = V2 ;
      Deontic4 = VS ;
      Deontic5 = VS ;
      Deontic6 = V2 ;
      Party1Action1 = V2 ;
      Party1Action2 = VP ;
      Party1Action3 = V2 ;
      Party2Action1 = V2 ;
      Party2Action2 = V2 ;
      Party2Action3 = VP ;
      Party2Action4 = V2 ;
      Party2Action5 = V2V ;
      Party2Action6 = VP ;
      Party2Action7 = V2V ;
      Party2Action8 = V2 ;
      TimeExpression1 = Adv ;
      TimeExpression2 = Adv ;
      TimeExpression3 = Adv ;
      TimeExpression4 = Subj ;
      TimeExpression5 = Adv ;
      TimeExpression6 = Adv ;
      TimeExpression7 = Adv ;
      TimeExpression8 = A2 ;
      TimeExpression9 = Adv ;
      TimeExpression10 = Adv ;
      TimeExpression11 = Adv ;
      ControlFlow1 = Adv ;
      ControlFlow2 = Adv ;
      ControlFlow3 = Adv ;
      ControlFlow4 = Adv ;
      ControlFlow5 = Adv ;

    lin
--sentence constructions
      --sentence 1
      aTransition timeexpression1 = mkUtt timeexpression1 ;
      bObligation party2 deontic1 object1 timeexpression2 = mkUtt (mkS (mkS (mkCl party2 deontic1 object1)) timeexpression2) ;
      cViolation controlflow1 party2 party2action1 object2 = mkUtt (mkS controlflow1 (mkS (mkCl party2 party2action1 object2))) ;
      --sentence2
      dTransition timeexpression3 = mkUtt timeexpression3 ;
      eTerm object3 object3definition = mkUtt (mkS (mkCl (object3) (object3definition))) ;
      fObligation object4 object4definition = mkUtt (mkS (mkCl (object4) (object4definition))) ;
      gTerm object5 object5definition = mkUtt (mkS (mkCl (object5) (object5definition))) ;
      --sentence 3
      hTransition timeexpression4 object5 object5definition = mkUtt (mkS timeexpression4 (mkS (mkCl object5 object5definition))) ;
      iPermission party1 deontic2 object1 timeexpression5 = mkUtt (mkS (mkS (mkCl party1 deontic2 object1)) timeexpression5) ;
      jAlternative controlflow1 object5 party2action3 = mkUtt (mkS controlflow1 (mkS (mkCl object5 party2action3))) ;
      kAlternativeCondition controlflow2 object6 party2action4 object3 = mkUtt (mkS controlflow2 (mkS (mkCl object6 party2action4 object3))) ;
      --sentence 4
      lTransition timeexpression9 = mkUtt timeexpression9 ;
      mObligation party2 deontic3 party1 timeexpression6 = mkUtt (mkS (mkS (mkCl party2 deontic3 party1)) timeexpression6) ;
      nViolation controlflow1 party2 party2action1 object7 = mkUtt (mkS controlflow1 (mkS (mkCl party2 party2action1 object7))) ;
      --sentence 5
      oCondition controlflow2 party2 party2action1 object7 = mkUtt (mkS controlflow2 (mkS (mkCl party2 party2action1 object7))) ;
      pCondition controlflow3 party2 party2action1 object2 = mkUtt (mkS controlflow3 (mkS (mkCl party2 party2action1 object2))) ;
      qCondition controlflow3 party2 party2action5 party1 party1action2 = mkUtt (mkS controlflow3 (mkS (mkCl party2 party2action5 party1 (party1action2)))) ;
      rConditionTrue controlflow4 object4 object4definition2 = mkUtt (mkS controlflow4 (mkS (mkCl object4 object4definition2))) ;
      sConditionTrue controlflow5 object8 party2action6 = mkUtt (mkS controlflow5 (mkS (mkCl object8 party2action6))) ;
      --sentence 6
      tTransition timeexpression10 party2 party2action7 party1 party1action2 = mkUtt (mkS timeexpression10 (mkS (mkCl party2 party2action7 party1 (party1action2)))) ;
      uPermission party2 deontic4 timeexpression7 = mkUtt (mkS (mkCl party2 deontic4 (timeexpression7))) ;
      vPermission party2 deontic5 timeexpression7 = mkUtt (mkS (mkCl party2 deontic5 (timeexpression7))) ;
      --sentence 7
      wTransition timeexpression11 = mkUtt timeexpression11 ;
      xObligation controlflow4 party1 deontic6 object9 = mkUtt (mkS controlflow4 (mkS (mkCl party1 deontic6 object9))) ;
      yObligationCondition controlflow2 object6 party2action4 object3 = mkUtt (mkS controlflow2 (mkS (mkCl object6 party2action4 object3))) ;
      zAlternative controlflow1 object5 object5definition = mkUtt (mkS controlflow1 (mkS (mkCl object5 object5definition))) ;
      zaAlternativeCondition controlflow2 object6 timeexpression8 object3 = mkUtt (mkS controlflow2 (mkS (mkCl object6 timeexpression8 object3))) ;
      --sentence 8
      zbCondition controlflow2 party1 party1action3 object9 = mkUtt (mkS controlflow2 (mkS (mkCl party1 party1action3 object9))) ;
      zcConditionTrue controlflow4 object10 object4 = mkUtt (mkS controlflow4 (mkS (mkCl object10 object4))) ;
      zdConditionTrue controlflow5 object4 object4definition2 = mkUtt (mkS controlflow5 (mkS (mkCl object4 object4definition2))) ;
      zeConditionTrue controlflow5 object8 party2action6 = mkUtt (mkS controlflow5 (mkS (mkCl object8 party2action6))) ;
--constructing noun phrase, verb phrases adverbial phrases. Prepositional phrasesthanks to Inari :) for the Upon parameters)
      Customer = mkNP the_Det (mkCN (mkN "customer")) ;
      Restaurant = mkNP the_Det (mkCN (mkN "restaurant")) ;
      MonsterBurger = mkNP the_Det (mkCN (mkA "monster") (mkN "burger")) ;
      PromptServeGuarantee = mkNP (mkDet it_Pron) (mkCN (mkN "prompt serve guarantee")) ;
      ChallengeEndTime = mkNP the_Det (mkN "challenge end time") ;
      CurrentTimePlus1 = mkVP (mkNP (mkCN (mkN "current time plus 1 hour"))) ;
      AmountOwing = mkNP the_Det (mkCN (mkN "amount owing")) ;
      FiftyDollars = mkVP (mkNP (mkCN (mkN "set to S$50"))) ;
      ZeroDollar = mkVP (mkNP (mkCN (mkN "set to S$0"))) ;
      TheChallenge = mkNP the_Det (mkCN (mkN "challenge")) ;
      Continue = progressiveVP (mkVP (mkV "continue")) ;
      Time = mkNP the_Det (mkCN (mkN "time")) ;
      PromptCheckGuarantee = mkNP (mkDet it_Pron) (mkCN (mkN "prompt check guarantee")) ;
      Contract = mkNP the_Det (mkCN (mkN "contract")) ;
      Bill = mkNP the_Det (mkCN (mkN "bill")) ;
      AmountPaid = mkNP the_Det (mkCN (mkN "amount paid")) ;
      ShouldServe = mkV2 "should serve" ;
      MayFinish = mkV2 "may finish" ;
      ShouldCheck = mkV2 "should check" ;
      MayConfirm = mkVS (mkV "may confirm") ;
      MayDisconfirm = mkVS (mkV "may disconfirm") ;
      MustPay = mkV2 "must pay" ;
      Order = mkV2 "order" ;
      Finish = mkVP (mkV "has finished") ;
      Pay = mkV2 "pay" ;
      Violate = mkV2 "violate" ;
      Serve = mkV2 "serve" ;
      Over = mkVP (mkV "is over") ;
      Reach = mkV2 "reach" ;
      Confirm = mkV2V (mkV "confirm") noPrep noPrep  ;
      Fulfill = mkVP (mkV "fulfill") ;
      Check = mkV2V (mkV "check") noPrep noPrep ;
      Disconfirm = mkV2 "disconfirm" ;
      Upon1 customer order burger =
         let order_burger : Adv = E.GerundAdv (mkVP order burger) ;
             customer_order_burger : NP = mkNP customer order_burger ;
          in SyntaxEng.mkAdv (mkPrep "Upon") customer_order_burger ;
      Within15minutes = mkAdv "within 15 minutes" ;
      Upon2 restaurant serve burger =
         let serve_burger : Adv = E.GerundAdv (mkVP serve burger) ;
             restaurant_serve_burger : NP = mkNP restaurant serve_burger ;
          in SyntaxEng.mkAdv (mkPrep "Upon") restaurant_serve_burger ;
      Whenever = mkAdv "Whenever" ;
      BeforeChallengeEndTime = mkAdv "before challenge end time" ;
      Upon3 =  SyntaxEng.mkAdv (mkPrep "Upon") ChallengeEndTime ;
      Upon4 = SyntaxEng.mkAdv (mkPrep "Upon") Restaurant ;
      Upon5 restaurant disconfirm customer =
        let disconfirm_customer : Adv = E.GerundAdv (mkVP disconfirm customer) ;
             restaurant_disconfirm_customer : NP = mkNP restaurant disconfirm_customer ;
         in SyntaxEng.mkAdv (mkPrep "Upon") restaurant_disconfirm_customer ;
      Within10minutes = mkAdv "within 10 minutes" ;
      Immediately = mkAdv "immediately" ;
      Before = mkA2 "before" "the" ;
      Otherwise = mkAdv "otherwise" ;
      If = mkSubj "If" ;
      Or = mkSubj "or" ;
      Then = mkSubj "then" ;
      And = mkSubj "and" ;
}





