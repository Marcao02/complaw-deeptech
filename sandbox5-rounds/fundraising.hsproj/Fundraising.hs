
module Fundraising where

import Currency
import Currency.Rates
import qualified Data.Maybe as Maybe
import qualified Data.ISO3166_CountryCodes as Country
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock    as Clock
import Text.Printf
import Security


ymd = Calendar.fromGregorian

data Locale = Locale { localeCountry :: Country.CountryCode -- this should be Country
                     , localeLang :: NaturalLanguage }
            deriving (Show)

data NaturalLanguage = English | Italian | French
                     deriving (Show)
naturalLanguages = [English, French]

class Lang a where
  title, header, to :: NaturalLanguage -> a -> String

class Named thing where
  nameOf :: thing -> String

locale = Locale { localeCountry = Country.SG
                , localeLang    = English }

data Round = Round { roundName :: String
                   , tranches :: [Tranche]
                   }
           deriving (Show)

instance Lang Round where
  title English round = "Investment Round"
  title Italian round = "Serie Investimento"
  title French  round = "Séries d'Investissement"
  header lang   round   = unwords ["#", title lang round ++ ":", nameOf round]
  to   lang   round = unlines [header lang round,
                                concat (map (to lang) (tranches round))]

instance Named Round where
  nameOf = roundName

data FundraisingTarget = FundraisingRange { low :: Money
                                          , med :: Money
                                          , high :: Money
                                          }
                       | FundraisingExactly Money
                       deriving (Show)
instance Lang FundraisingTarget where
  title  _ frt = ""
  header _ frt = ""
  to English (FundraisingExactly money) = "raising " ++ to English money
  to English frt    = unwords ["raising between",          to English (low frt),
                               "and",                      to English (high frt),
                               "with a mid-range goal of", to English (med frt)]
  to _ frt = "xxx"
    
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

instance Lang Tranche where
  title English tranche = "Tranche"
  title Italian tranche = "Tranche di Investimento"
  title French  tranche = "Tranche d'Investissement"
  header   lang tranche   = unlines [unwords ["##", title lang tranche ++ ":", nameOf tranche]]
  to       lang tranche   = unlines [header lang tranche,
                                     unlines ["This is a description of a tranche",
                                              to lang (fundraisingTarget tranche),
                                              "the deadline is " ++ (Calendar.showGregorian $ deadline tranche)
                                              ]]

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
    |      redeemable sec && Maybe.isJust    (term sec)                       = Debt
    |      redeemable sec && Maybe.isNothing (term sec)                       = Preferred
    |      redeemable sec && any preferredRight (rights sec)                  = Preferred
    | not (redeemable sec)                                                   = Ordinary
    | otherwise                                                              = Unknown
   where
    preferredRight (Voting 1)     = False
    preferredRight (Voting 0)     = False
    preferredRight (Voting _)     = True
    preferredRight Board        = True
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

instance Lang Money where
  title  _ _ = ""
  header _ _ = "" -- in future change header to markup and add a character style
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
            , securityTemplate = Security { preMoney = Nothing
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
  putStrLn $ unlines $ [ to lang round | round <- myDefaults, lang <- naturalLanguages ]
--  putStrLn $ show myDefaults
  
