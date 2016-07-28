import Currency
import Currency.Rates
import qualified Data.Maybe as Maybe
import qualified Data.ISO3166_CountryCodes as Country
import qualified Data.Time.Calendar as Calendar
import Text.Printf

ymd = Calendar.fromGregorian

data Locale = Locale { localeCountry :: String -- this should be Country
                     , localeLang :: String }
            deriving (Show)

locale = Locale { localeCountry = "SG"
                , localeLang    = "EN" }

data Round = Round { roundName :: String
                   , tranches :: [Tranche]
                   }
           deriving (Show)

data FundraisingTarget = FundraisingRange { low :: Money
                                          , med :: Money
                                          , high :: Money
                                          }
                       | FundraisingExactly Money
                       deriving (Show)
                                
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

data SecurityNature = Ordinary | Preferred String | Debt
instance Show SecurityNature where
  show Ordinary = "Ordinary Shares"
  show (Preferred x) = "Preferred " ++ x
  show Debt = "Debt"

-- we don't specify if it's debt or equity -- the nature of the security is interpretive
data Security = Security { preMoney :: Maybe Money
                         , discount :: Maybe Int -- numerator /100
                         , term :: Maybe NumMonths -- TODO: change this to a dateinterval
                         , nature :: SecurityNature
                         }
instance Show Security where
  show security = show (nature security)

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
instance Show Money where
  show (Money { currency=cu, cents=q }) =
      printf "%s %d%s"
             (show cu)
                (lchunk q (minorUnits cu))
                (rchunk q (minorUnits cu))
   where
    lchunk q Nothing = q
    lchunk q (Just digits) = q `quot` (toE digits)
    toE digits = truncate (10**(fromIntegral digits))
    rchunk q Nothing = show q
    rchunk q b = showcents b (cents b q)
    showcents Nothing _ = ""
    showcents _ Nothing = ""
    showcents (Just 0) (Just c) = ""
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
                                          , nature = Ordinary
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
                                          , nature = Debt
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
                                          , nature = Debt
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
                                          , nature = Debt
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
                                          , nature = Debt
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
                                          , nature = Debt
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
                                          , nature   = Debt
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
                                          , nature   = Debt
                                          }
            , businessGoals = ["build v2.x"
                              ,"conduct revenue model experiments"
                              ,"achieve product/market fit"]
              }
          ]
      }
  ]

