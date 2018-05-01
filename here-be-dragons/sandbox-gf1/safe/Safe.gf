abstract Safe = {
 
  flags startcat = Contract ;
  
  cat
    Contract ;
    Action ;
   -- Event ;
   -- Obligation ;    
   -- Condition ;

  fun
    
 --    Safe_01 : Action -> Event -> Obligation -> Condition -> Contract ;    
     certify : Action ; -- this certifies that in exchange for
     --investor : Event ; -- the payment by John (the " Investor") of $100,000 (the "Purchase Amount"} on or about yesterday
     --company : Obligation ; -- the company, a Latvian corporation (the "Company"), hereby issues to the Investor the right to certain shares of the Company's capital stock
     --subject_to : Condition ; -- subject to the terms set forth below
     
} 












--a bunch of failed codes listed below
--for parsing:
--This certifies that in exchange for - done
--the payment by John of 100 euros on or about yesterday
--the company a Latvian corporation hereby issues to the Investor the right to certain shares of the Company's capital stock
--subject to the terms set forth below - done

                                                                               
--Action: 
--UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetNP (DetQuant this_Quant NumSg)) (AdvVP (AdvVP (UseV certify_V) (AdAdv that_AdA (PrepNP in_Prep (MassNP (UseN exchange_N))))) for_Adv)))

--Condition: 
--UttAdv (PrepNP subject_to_Prep (AdvNP (AdvNP (PPartNP (DetCN the_Det (UseN terms_N)) set_forth_V2) forth_Adv) below_Adv))


    
-- This certifies that in exchange for -- action
      -- the payment by John (the " Investor") of $100,000 (the "Purchase Amount"} on or about yesterday, -- event
     -- the company, a Latvian corporation (the "Company"), hereby issues to the Investor the right to certain shares of the Company's capital stock, -- obligation
          -- subject to the terms set forth below.

-- Test 01
-- This certifies that in exchange for the payment 
     -- by InvestorName (the " Investor") of $100,000 (the "Purchase Amount"}
          -- on or about DateOfSafe,
     -- Company, a StateOfIncorporation corporation (the "Company"),
          -- hereby issues to the Investor the right to certain shares of the Company's capital stock,
          -- subject to the terms set forth below.

-- deprecated Det ; CN ; NP ; Conj ; Predet ; CN ; Adj ; Adv ;  
-- deprecated DetCN     : Det -> CN -> NP ;
 
-- mkCl : NP -> VP -> Cl -- API: mkUtt (mkCl she_NP (mkVP always_AdV (mkVP sleep_V))) -- this certifies
-- mkAdv : Prep -> NP -> Adv -- mkUtt (mkAdv in_Prep (mkNP the_Det house_N)) -- in the house
-- for_Prep : Prep -- mkUtt (mkAdv for_Prep it_NP) -- for it
-- by8agent_Prep : Prep -- mkUtt (mkAdv by8agent_Prep it_NP) -- by it
-- possess_Prep : Prep --  mkUtt (mkAdv possess_Prep it_NP) -- of it 
-- mkDigits : Dig -> Digits -> Digits  -- mkUtt (mkCard (mkDigits n1_Dig (mkDigits n2_Dig (mkDigits n3_Dig (mkDigits n3_Dig (mkDigits n4_Dig (mkDigits n8_Dig (mkDigits n6_Dig)))))))) -- 100,000
-- mkAdv : Conj -> Adv -> Adv -> Adv -- mkUtt (mkAdv and_Conj here_Adv now_Adv) -- on or about
-- dayMonthYearAdv : Monthday -> Month -> Year -> Adv -- dayMonthYearAdv dm y = ParadigmsEng.mkAdv ("on or about" ++ d.s ! R.NPAcc ++ m.s ! R.Sg ! R.Nom ++ y.s ! R.NPAcc)  -- on 17 March 1980
-- the_Det : Det --  mkUtt (mkNP the_Det house_N) -- the Company
-- mkNP : Quant -> CN -> NP -- mkUtt (mkNP this_Quant (mkCN old_A man_N)) -- a StateOfIncorporation corporation (the "Company")
-- hereby (have to manually define)
-- issues to the investor ???
-- the right to ???
-- mkCN : N2 -> NP -> CN  -- mkUtt (mkCN mother_N2 (mkNP the_Det king_N)) -- certain shares of the company's capital stock
-- subject to -- dependent/conditional adj
-- the_Det : Det --  mkUtt (mkNP the_Det house_N) -- the terms
-- set forth below -- set is verb forth is adverb

-- mkUtt (mkS today_Adv (mkS (mkCl she_NP sleep_V)))
-- mkSC (mkS (mkCl she_NP sleep_V))

-- mkCl : NP -> VS -> S -> Cl

-- mkCl : NP -> VP -> Cl
-- mkVP : VS -> S -> VP
-- mkVP : VP -> Adv -> VP

-- api: mkUtt (mkCl she_NP say_VS (mkS (mkCl i_NP sleep_V)))
-- she says that i sleep

-- mkS : mkAdv -> S -> S 
--mkUtt (mkS today_Adv (mkS (mkCl she_NP sleep_V)))

--for parsing:
--This certifies that in exchange for - done
--the payment by John of 100 euros on or about yesterday
--the company a Latvian corporation hereby issues to the Investor the right to certain shares of the Company's capital stock
--subject to the terms set forth below - done

                                                                               
--Action: 
--UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetNP (DetQuant this_Quant NumSg)) (AdvVP (AdvVP (UseV certify_V) (AdAdv that_AdA (PrepNP in_Prep (MassNP (UseN exchange_N))))) for_Adv)))

--Condition: 
--UttAdv (PrepNP subject_to_Prep (AdvNP (AdvNP (PPartNP (DetCN the_Det (UseN terms_N)) set_forth_V2) forth_Adv) below_Adv))