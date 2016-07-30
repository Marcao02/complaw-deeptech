
module Fundraising where

import Currency
import Currency.Rates
import qualified Data.Maybe as Maybe
import qualified Data.ISO3166_CountryCodes as Country
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock    as Clock
import qualified NaturalLanguage    as NL
import Text.Printf
import Security


ymd = Calendar.fromGregorian

data Locale = Locale { localeCountry :: Country.CountryCode -- this should be Country
                     , localeLang :: NL.Tongue }
            deriving (Show)

class Heady a where
  title, header :: NL.Ctx -> a -> String

class Named thing where
  nameOf :: thing -> String

locale = Locale { localeCountry = Country.SG
                , localeLang    = NL.English }

data Round = Round { roundName :: String
                   , tranches :: [Tranche]
                   }
           deriving (Show)

instance Heady Round where
  title (NL.Ctx { NL.lang=NL.English }) round = "Investment Round"
  title (NL.Ctx { NL.lang=NL.Italian }) round = "Serie Investimento"
  title (NL.Ctx { NL.lang=NL.French  }) round = "Séries d'Investissement"
  header lang   round   = unwords ["#", title lang round ++ ":", nameOf round]


instance NL.Lang Round where
  to   lang   round = unlines [header lang round,
                                concat (map (NL.to lang) (tranches round))]

instance Named Round where
  nameOf = roundName

data FundraisingTarget = FundraisingRange { low :: Money
                                          , med :: Money
                                          , high :: Money
                                          }
                       | FundraisingExactly Money
                       deriving (Show)
instance Heady FundraisingTarget where
  title  _ frt = ""
  header _ frt = ""

instance NL.Lang FundraisingTarget where
  to lang@(NL.Ctx { NL.lang=NL.English }) (FundraisingExactly money) = "raising " ++ NL.to lang money
  to lang@(NL.Ctx { NL.lang=NL.English }) frt    = unwords ["raising between",          NL.to lang (low frt),
                               "and",                      NL.to lang (high frt),
                               "with a mid-range goal of", NL.to lang (med frt)]
  to _ _ = "à venir bientôt, je vous remercie de votre patience"
    
data Tranche = Tranche { trancheName :: String
                       , milestones :: [Milestone]
                       , fundraisingTarget :: FundraisingTarget
                       , deadline :: Calendar.Day
                       , securityTemplate :: Security
                       , businessGoals :: [BusinessGoal]
                       }
               deriving (Show)

type BusinessGoal = String
type Milestone = (BusinessGoal, Bool)
type NumDays = Int
type NumMonths = Int

instance Heady Tranche where
  title (NL.Ctx { NL.lang=NL.English }) tranche = "Tranche"
  title (NL.Ctx { NL.lang=NL.Italian }) tranche = "Tranche di Investimento"
  title (NL.Ctx { NL.lang=NL.French }) tranche = "Tranche d'Investissement"
  header   lang tranche   = unlines [unwords ["##", title lang tranche ++ ":", nameOf tranche]]

instance NL.Lang Tranche where
  to lang tranche = unlines [header lang tranche,
                             unlines [NL.to lang (fundraisingTarget tranche),
                                      "the deadline is " ++ (Calendar.showGregorian $ deadline tranche),
                                      sayPreMoney lang $ preMoney $ securityTemplate tranche
                                     ]]

sayPreMoney :: NL.Ctx -> Maybe Money -> String
sayPreMoney lang (Just m) = "pre-money cap of " ++ NL.to lang m
sayPreMoney lang Nothing  = "(no pre-money cap)"

-- pastFuture :: Calendar.Day -> Calendar.Day -> String
-- TODO: figure out if the deadline is in the future or the past relative to the invocation time of the script. we probably need a state monad to wrap IO in.

instance Named Tranche where
  nameOf = trancheName

-- later we will have security combinators, to represent an instrument whose attributes may change based on circumstance
-- for GAAP / IFRS purposes we want to be able to characterize both individual simple Securities and combinations of securities.
-- a security combinator may be threaded together with a modal combinator.

data Right = Voting Int | Board | OtherRight String
             deriving (Show)

-- if we don't specify if it's debt or equity, the nature of the security is interpretive
data Security = Security { preMoney        :: Maybe Money
                         , discount        :: Maybe Int -- numerator /100
                         , term            :: Maybe NumMonths -- TODO: change this to a dateinterval
                         , redeemable      :: Bool
                         , rights          :: [Right]
                         , natureHardcoded :: Maybe SecurityNature
                         }
              deriving (Show)

data SecurityNature = Ordinary | Preferred | Debt | Equity | Unknown
                    deriving (Show)
securityNature (Security { natureHardcoded = Just sn }) = sn
securityNature sec
    |      redeemable sec && Maybe.isJust    (term sec)      = Debt
    |      redeemable sec && Maybe.isNothing (term sec)      = Preferred
    |      redeemable sec && any preferredRight (rights sec) = Preferred
    | not (redeemable sec)                                   = Ordinary
    | otherwise                                              = Unknown
   where
    preferredRight (Voting 1)     = False
    preferredRight (Voting 0)     = False
    preferredRight (Voting _)     = True
    preferredRight Board          = True
    preferredRight (OtherRight _) = True

-- Money
-- 
-- Money { currency = jpy), cents = 12345 }
-- JPY 12345
-- 
-- Money { currency = sgd), cents = 12345 }
-- SGD 123.45

sgd = ISO4217Currency (NationalCurrency Country.SG 'D')
usd = ISO4217Currency (NationalCurrency Country.US 'D')
jpy = ISO4217Currency (NationalCurrency Country.JP 'Y')

currency ¢ cents = Money { currency = currency, cents = cents }

data Money = Money { currency :: Currency
                   , cents :: Int
                   }
  deriving (Show)

instance Heady Money where
  title  _ _ = ""
  header _ _ = "" -- in future change header to markup and add a character style

instance NL.Lang Money where
  to _ (Money { currency=cu, cents=q }) =
      printf "%s %s%s"
             (show cu)
                (commafy $ lchunk q (minorUnits cu))
                (rchunk q (minorUnits cu))
   where
    lchunk q Nothing = q
    lchunk q (Just digits) = q `quot` (toE digits)
    toE digits = truncate (10**(fromIntegral digits))
    rchunk q Nothing = show q
    rchunk q b = showcents b (cents b q)
    showcents Nothing _ = ""
    showcents _ Nothing = ""
    showcents (Just 0) _ = ""
    showcents _ (Just 0) = ""
    showcents (Just digits) (Just c) = "." ++ printf ("%0"++(show digits)++"d") c
    cents a b = do
      mi <- a
      return ((abs b) `mod` (toE mi))

--
--
--                         

myDefaults = [
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
            , securityTemplate = Security { preMoney = Just $ sgd ¢ 100
                                          , discount = Nothing
                                          , term = Nothing
                                          , redeemable = False
                                          , rights = [Voting 1]
                                          , natureHardcoded = Nothing
                                          }
            , businessGoals = ["Build v1 MVP"]
              }
          ]
      },
  Round {
      roundName = "loans from director", tranches = [
          Tranche {
              trancheName = "airfare 1"
            , milestones = [ ("Long Nguyen ready to visit",    True) ]
            , fundraisingTarget = FundraisingExactly $ sgd ¢ 207761
            , deadline = ymd 2016 2 21
            , securityTemplate = Security { preMoney = Nothing
                                          , discount = Just 0
                                          , term = Just 36
                                          , redeemable = True
                                          , rights = []
                                          , natureHardcoded = Nothing
                                          }
            , businessGoals = ["support Legalese summit"]
              },
          Tranche {
              trancheName = "airfare 2"
            , milestones = [ ("Anuj Gupta ready to visit",    True) ]
            , fundraisingTarget = FundraisingExactly $ sgd ¢ 70000
            , deadline = ymd 2016 2 21
            , securityTemplate = Security { preMoney = Nothing
                                          , discount = Just 0
                                          , term = Just 36
                                          , redeemable = True
                                          , rights = []
                                          , natureHardcoded = Nothing
                                          }
            , businessGoals = ["support Legalese summit"]
              },
          Tranche {
              trancheName = "airfare 3"
            , milestones = [ ("Yochi ready to visit",    True) ]
            , fundraisingTarget = FundraisingExactly $ sgd ¢ 84877
            , deadline = ymd 2016 2 21
            , securityTemplate = Security { preMoney = Nothing
                                          , discount = Just 0
                                          , term = Just 36
                                          , redeemable = True
                                          , rights = []
                                          , natureHardcoded = Nothing
                                          }
            , businessGoals = ["support Legalese summit"]
              },
          Tranche {
              trancheName = "fossasia 2016"
            , milestones = [ ("tickets for FOSSasia",    True) ]
            , fundraisingTarget = FundraisingExactly $ sgd ¢ 46500
            , deadline = ymd 2016 2 21
            , securityTemplate = Security { preMoney = Nothing
                                          , discount = Just 0
                                          , term = Just 36
                                          , redeemable = True
                                          , rights = []
                                          , natureHardcoded = Nothing
                                          }
            , businessGoals = ["support Legalese summit"]
              },
          Tranche {
              trancheName = "registration of legalese.com"
            , milestones = [ ("contract of transfer",    True) ]
            , fundraisingTarget = FundraisingExactly $ sgd ¢ 502557
            , deadline = ymd 2016 2 21
            , securityTemplate = Security { preMoney = Nothing
                                          , discount = Just 0
                                          , term = Just 36
                                          , redeemable = True
                                          , rights = []
                                          , natureHardcoded = Nothing
                                          }
            , businessGoals = ["support Legalese summit"]
              }
          ] },
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
            , securityTemplate = Security { preMoney = Just $ sgd ¢ 350000000
                                          , discount = Nothing
                                          , term     = Just 36
                                          , redeemable = True
                                          , rights = []
                                          , natureHardcoded   = Nothing
                                          }
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
            , securityTemplate = Security { preMoney = Just $ sgd ¢ 600000000
                                          , discount = Nothing
                                          , term     = Just 36
                                          , redeemable = True
                                          , rights = []
                                          , natureHardcoded   = Nothing
                                          }
            , businessGoals = ["build v2.x"
                              ,"conduct revenue model experiments"
                              ,"achieve product/market fit"]
              }
          ]
      }
  ]

main = do
  currentTime <- Clock.getCurrentTime
  let currentDay = Clock.utctDay currentTime
  putStrLn $ "UTCtime = " ++ Calendar.showGregorian currentDay
  putStrLn $ unlines $ [ NL.to lang round | lang <- [NL.Ctx { NL.lang=NL.English, NL.tense=NL.Present }], round <- myDefaults ]
--  putStrLn $ show myDefaults
  
