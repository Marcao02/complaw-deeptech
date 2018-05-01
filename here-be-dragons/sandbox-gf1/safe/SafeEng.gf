concrete SafeEng of Safe = open ParadigmsEng, SyntaxEng, IrregEng, (D=DictionaryEng) in {

  lincat
    Contract = Text ;
    Action = Cl ;
    --Event = NP ;
   -- Obligation = VP ;
   -- Condition = Adv  ;

  lin
    --Safe_01 action event obligation condition = action event obligation condition ;
    
--  (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetNP (DetQuant this_Quant NumSg)) (AdvVP (AdvVP (UseV certify_V) (AdAdv that_AdA (PrepNP in_Prep (MassNP (UseN exchange_N))))) for_Adv))) ;

-- mkCl    (mkCl (mkNP (mkDet this_Quant )) (mkVP (mkVP (mkVP certify_V) (mkAdv that_AdA (mkAdv in_Prep (mkNP exchange_N)))) for_Adv))

    certify = (mkCl (mkNP (mkDet this_Quant)) 
            (mkVP (mkVP (mkVP D.certify_V) 
            (SyntaxEng.mkAdv D.that_AdA (SyntaxEng.mkAdv in_Prep (mkNP D.exchange_N)))) D.for_Adv)) ;
}

 --oper
    --mkAction : NP -> VP -> Adv -> Cl =
    --  \

    --mkEvent =
    --mkOblig = 
    --mkCondition = UttAdv (PrepNP subject_to_Prep (AdvNP (AdvNP (PPartNP (DetCN the_Det (UseN terms_N)) set_forth_V2) forth_Adv) below_Adv))
    


--for parsing:
--This certifies that in exchange for - done
--the payment by John of 100 euros on or about yesterday
--the company a Latvian corporation hereby issues to the Investor the right to certain shares of the Company's capital stock
--subject to the terms set forth below - done

                                                                               
--Action: 
--UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetNP (DetQuant this_Quant NumSg)) (AdvVP (AdvVP (UseV certify_V) (AdAdv that_AdA (PrepNP in_Prep (MassNP (UseN exchange_N))))) for_Adv)))

--Condition: 
--UttAdv (PrepNP subject_to_Prep (AdvNP (AdvNP (PPartNP (DetCN the_Det (UseN terms_N)) set_forth_V2) forth_Adv) below_Adv))


-- This certifies that in exchange for 
      -- the payment by John (the " Investor") of $100,000 (the "Purchase Amount"} on or about yesterday,
     -- the company, a Latvian corporation (the "Company"), hereby issues to the Investor the right to certain shares of the Company's capital stock,
          -- subject to the terms set forth below.


    -- Determiner = Det ;

-- John knows that in the car I eat cats
-- mkUtt ( mkS ( mkCl ( mkNP john_PN ) ( mkVP know_VS ( mkS ( mkAdv in_Prep ( mkNP the_Quant car_N ) ) ( mkS ( mkCl ( mkNP i_Pron ) eat_V2 ( mkNP a_Quant pluralNum cat_N ) ) ) ) ) ) )

-- try this simple form first:
-- mkCl : NP -> VS -> S -> Cl (she says that i sleep)
-- API : mkUtt (mkCl she_NP say_VS (mkS (mkCl i_NP sleep_V)))
-- she says that i sleep

