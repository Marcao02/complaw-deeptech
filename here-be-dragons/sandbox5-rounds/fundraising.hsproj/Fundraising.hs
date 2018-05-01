
module Fundraising where

import Currency
import Currency.Rates
import qualified Data.Maybe as Maybe
import qualified Data.ISO3166_CountryCodes as Country
import qualified Data.Time.Clock    as Clock
import qualified NaturalLanguage    as NL -- most of the output related code lives here
import Text.Printf
import Security
import Money
import qualified Data.Map as Map
import qualified Data.Time.Calendar as Calendar

-- ====================================================================== helper functions for output
--
-- Heady: titles and headers for output
--

class Heady a where
  title, header :: NL.Ctx -> a -> String

instance Heady Money where
  title  _ _ = ""
  header _ _ = "" -- in future change header to markup and add a character style


--
-- locale
--

locale = Locale { localeCountry = Country.SG
                , localeLang    = NL.English }

data Locale = Locale { localeCountry :: Country.CountryCode -- this should be Country
                     , localeLang :: NL.Tongue }
            deriving (Show)

--
-- time and date
--
ymd = Calendar.fromGregorian


-- ====================================================================== ROUND

data Round = Round { roundName :: String
                   , tranches :: [Tranche]
                   }
           deriving (Show)

instance Heady Round where
  title (NL.Ctx { NL.lang=NL.English }) round = "Investment Round"
  title (NL.Ctx { NL.lang=NL.Italian }) round = "Serie Investimento"
  title (NL.Ctx { NL.lang=NL.French  }) round = "Séries d'Investissement"
  header lang   round   = unwords ["#", title lang round ++ ":", NL.nameOf round]

instance NL.Named Round    where nameOf =    roundName

instance NL.Lang Round where
  to   lang   round = unlines [header lang round,
                                concat (map (NL.to lang) (tranches round))]

-- ---------------------------------------------------------------------- FundraisingTarget

data FundraisingTarget = FundraisingRange { low :: Money
                                          , med :: Money
                                          , high :: Money
                                          }
                       | FundraisingExactly Money
                       deriving (Show)
instance Heady FundraisingTarget where
  title  _ frt = ""
  header _ frt = ""

-- make this language-independent
frtBetween lang frr = unwords ["between",                 NL.to lang (low  frr),
                              "and",                      NL.to lang (high frr),
                              "with a mid-range goal of", NL.to lang (med  frr)]

frtAmount lang (FundraisingExactly money) = NL.to lang money
frtAmount lang frr   = frtBetween lang frr

instance NL.Lang FundraisingTarget where
  to lang@(NL.Ctx { NL.lang=NL.English }) frt = unwords [NL.v "raise" lang, frtAmount lang frt]
  to _ _ = "à venir bientôt, je vous remercie de votre patience"
    



-- ====================================================================== TRANCHE

data Tranche = Tranche { trancheName :: String
                       , milestones :: [Milestone]
                       , fundraisingTarget :: FundraisingTarget
                       , deadline :: Calendar.Day
                       , deadline_fundsTransfer :: Maybe Calendar.Day
                       , securityTemplate :: Security
                       , businessGoals :: [BusinessGoal]
                       }
               deriving (Show)

type BusinessGoal = String
type Milestone = (BusinessGoal, Bool)
type NumDays = Int

instance NL.Named Tranche  where nameOf =  trancheName

instance Heady Tranche where
  title (NL.Ctx { NL.lang=NL.English }) tranche = "Tranche"
  title (NL.Ctx { NL.lang=NL.Italian }) tranche = "Tranche di Investimento"
  title (NL.Ctx { NL.lang=NL.French }) tranche = "Tranche d'Investissement"
  header   lang tranche   = unlines [unwords ["##", title lang tranche ++ ":", NL.nameOf tranche]]

closeDate :: NL.Ctx -> String
closeDate lang@(NL.Ctx { NL.lang=NL.English, NL.tense=Just NL.Past    }) = "closed on"
closeDate lang@(NL.Ctx { NL.lang=NL.English, NL.tense=Just NL.Present }) = "closing on"
closeDate lang@(NL.Ctx { NL.lang=NL.English, NL.tense=Just NL.Future  }) = "will close on"
closeDate lang@(NL.Ctx { NL.lang=NL.English, NL.tense=Nothing      }) = "deadline"
  
instance NL.Lang Tranche where
  to lang' tranche = unlines [ header lang tranche
                             , NL.to lang (fundraisingTarget tranche)
                             , unwords [ closeDate lang, (Calendar.showGregorian $ deadline tranche)]
                             , maybe "" (\dl -> "funds to be transferred by " ++ NL.to lang dl) (deadline_fundsTransfer tranche)
                             , NL.to lang (securityTemplate tranche)
                             ]
    where lang = lang' { NL.tense = Just $ mkTense (deadline tranche, NL.refTime lang') }

mkTense :: (Calendar.Day,Calendar.Day) -> NL.Tense
mkTense (t1,t2)
  | t1 <  t2 = NL.Past
  | t1 == t2 = NL.Present
  | t1 >  t2 = NL.Future







-- ====================================================================== USER SPACE CONFIGURATION


s_simpleCN =
  Security { securityName = "Simplified Convertible Note"
           , preMoney = Nothing
           , discount = Just 0
           , term = Just 36
           , redeemable = True
           , rights = []
           , natureHardcoded = Nothing
           }
  
s_ordinary =
  Security { securityName = "Ordinary Shares"
           , preMoney = Nothing
           , discount = Nothing
           , term = Nothing
           , redeemable = False
           , rights = [Voting 1]
           , natureHardcoded = Nothing
           }
  
s_fixedCN =
  Security { securityName = "Fixed Convertible Note"
           , preMoney = Nothing
           , discount = Just 0
           , term = Just 36
           , redeemable = True
           , rights = []
           , natureHardcoded = Nothing
           }


-- ====================================================================== An Actual Company

  
myDefaults = [

-- ---------------------------------------------------------------------- Incorporation
  Round {
      roundName = "incorporation", tranches = [
          Tranche {
              trancheName = "incorporation"
            , milestones = [ ("need to own IP",    True)
                           , ("raising funding",   False)
                           , ("receiving revenue", False)
                           ]
            , fundraisingTarget = FundraisingExactly $ sgd ¢ 100
            , deadline = ymd 2015 7 1
            , deadline_fundsTransfer = Nothing
            , securityTemplate = s_ordinary { preMoney = Just $ sgd ¢ 0 }
            , businessGoals = ["Build v1 MVP"]
              }
          ]
      },

-- ---------------------------------------------------------------------- Loans from Director
  Round {
      roundName = "loans from director", tranches = [
          Tranche {
              trancheName = "airfare 1"
            , milestones = [ ("Long Nguyen ready to visit",    True) ]
            , fundraisingTarget = FundraisingExactly $ sgd ¢ 207761
            , deadline = ymd 2016 2 21
            , deadline_fundsTransfer = Nothing
            , securityTemplate = s_simpleCN
            , businessGoals = ["support Legalese summit"]
              },
          Tranche {
              trancheName = "airfare 2"
            , milestones = [ ("Anuj Gupta ready to visit",    True) ]
            , fundraisingTarget = FundraisingExactly $ sgd ¢ 70000
            , deadline = ymd 2016 2 21
            , deadline_fundsTransfer = Nothing
            , securityTemplate = s_simpleCN
            , businessGoals = ["support Legalese summit"]
              },
          Tranche {
              trancheName = "airfare 3"
            , milestones = [ ("Yochi ready to visit",    True) ]
            , fundraisingTarget = FundraisingExactly $ sgd ¢ 84877
            , deadline = ymd 2016 2 21
            , deadline_fundsTransfer = Nothing
            , securityTemplate = s_simpleCN
            , businessGoals = ["support Legalese summit"]
              },
          Tranche {
              trancheName = "fossasia 2016"
            , milestones = [ ("tickets for FOSSasia",    True) ]
            , fundraisingTarget = FundraisingExactly $ sgd ¢ 46500
            , deadline = ymd 2016 2 21
            , deadline_fundsTransfer = Nothing
            , securityTemplate = s_simpleCN
            , businessGoals = ["support Legalese summit"]
              },
          Tranche {
              trancheName = "registration of legalese.com"
            , milestones = [ ("contract of transfer",    True) ]
            , fundraisingTarget = FundraisingExactly $ sgd ¢ 502557
            , deadline = ymd 2016 2 21
            , deadline_fundsTransfer = Nothing
            , securityTemplate = s_simpleCN
            , businessGoals = ["support Legalese summit"]
              }
          ] },

-- ---------------------------------------------------------------------- Angel Round

  Round {
      roundName = "angel round ", tranches = [
          Tranche {
              trancheName = "1"
            , milestones = [ ("v1.0 MVP done",        True)
                           , ("problem/solution fit", True)
                           ]
            , fundraisingTarget = FundraisingRange { low  = sgd ¢ 20000000
                                                   , med  = sgd ¢ 25000000
                                                   , high = sgd ¢ 30000000
                                                   }
            , deadline = ymd 2016 8 4
            , deadline_fundsTransfer = Just $ ymd 2016 10 1
            , securityTemplate = s_fixedCN { preMoney = Just $ sgd ¢ 350000000 }
            , businessGoals = ["build v2.0"]
              },
          Tranche {
              trancheName = "2"
            , milestones = [ ("v2.0 built",           False)
                           , ("solution/product fit", False)
                           ]
            , fundraisingTarget = FundraisingRange { low  = sgd ¢  75000000
                                                   , med  = sgd ¢ 100000000
                                                   , high = sgd ¢ 130000000
                                                   }
            , deadline = ymd 2017 2 14
            , deadline_fundsTransfer = Just $ ymd 2017 4 1
            , securityTemplate = s_fixedCN { preMoney = Just $ sgd ¢ 600000000 }
            , businessGoals = ["build v2.x"
                              ,"conduct revenue model experiments"
                              ,"achieve product/market fit"]
              }
          ]
      }
  ]

-- ====================================================================== MAIN

main = do
  currentDay <- fmap Clock.utctDay Clock.getCurrentTime
  putStrLn $ "as of " ++ Calendar.showGregorian currentDay
  putStrLn $ unlines $ [ NL.to
                         (NL.Ctx { NL.lang = natlang
                                 , NL.refTime = currentDay
                                 , NL.tense = Nothing
                                 })
                         round
                         | natlang <- [NL.English]
                         , round   <- [ last myDefaults ] ]
--  putStrLn $ show myDefaults
  
