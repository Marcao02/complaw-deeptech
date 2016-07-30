
module Fundraising where

import Currency
import Currency.Rates
import qualified Data.Maybe as Maybe
import qualified Data.ISO3166_CountryCodes as Country
import qualified Data.Time.Clock    as Clock
import qualified NaturalLanguage    as NL
import Text.Printf
import Security
import qualified Data.Time.Calendar as Calendar; ymd = Calendar.fromGregorian

data Locale = Locale { localeCountry :: Country.CountryCode -- this should be Country
                     , localeLang :: NL.Tongue }
            deriving (Show)

class Heady a where
  title, header :: NL.Ctx -> a -> String

class Named thing       where nameOf :: thing -> String
instance Named Round    where nameOf =    roundName
instance Named Tranche  where nameOf =  trancheName
instance Named Security where nameOf = securityName


locale = Locale { localeCountry = Country.SG
                , localeLang    = NL.English }




-- ====================================================================== ROUND

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
type NumMonths = Int

instance Heady Tranche where
  title (NL.Ctx { NL.lang=NL.English }) tranche = "Tranche"
  title (NL.Ctx { NL.lang=NL.Italian }) tranche = "Tranche di Investimento"
  title (NL.Ctx { NL.lang=NL.French }) tranche = "Tranche d'Investissement"
  header   lang tranche   = unlines [unwords ["##", title lang tranche ++ ":", nameOf tranche]]

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






-- ====================================================================== SECURITY

-- later we will have security combinators, to represent an instrument whose attributes may change based on circumstance
-- for GAAP / IFRS purposes we want to be able to characterize both individual simple Securities and combinations of securities.
-- a security combinator may be threaded together with a modal combinator.

data Right = Voting Int | Board | OtherRight String
             deriving (Show)

data Security = Security { securityName    :: String
                         , preMoney        :: Maybe Money
                         , discount        :: Maybe Int -- numerator /100
                         , term            :: Maybe NumMonths -- TODO: change this to a dateinterval | fixedDate
                         , redeemable      :: Bool
                         , rights          :: [Right]
                         , natureHardcoded :: Maybe SecurityNature
                         }
              deriving (Show)

--
-- TODO: add national and linguistic context to a security
-- because a security is scoped nationally
-- we probably need to be clever about AbstractSecurity vs a NationalSecurity vs a ConcreteSecurity which has a specific contract with parties
-- 

instance NL.Lang Security where
 to lang security = unlines [ "Security: " ++ nameOf security ++ " (" ++ NL.to lang (securityNature security) ++ ")"
                            , sayPreMoney lang $ preMoney security
                            ]
   
sayPreMoney :: NL.Ctx -> Maybe Money -> String
sayPreMoney lang (Just m) = "pre-money cap of " ++ NL.to lang m
sayPreMoney lang Nothing  = "(no pre-money cap)"


instance NL.Lang Calendar.Day where
  to lang@(NL.Ctx { NL.lang=NL.English }) = show
  to _ = show


-- ---------------------------------------------------------------------- SecurityNature

-- if we don't specify if it's debt or equity, the nature of the security is interpretive
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

instance NL.Lang SecurityNature where
  to lang@(NL.Ctx { NL.lang=NL.English }) Ordinary = "ordinary shares"
  to lang@(NL.Ctx { NL.lang=NL.English }) Preferred = "preferred shares"
  to lang@(NL.Ctx { NL.lang=NL.English }) Debt = "debt"
  to lang@(NL.Ctx { NL.lang=NL.English }) Equity = "equity"
  to lang@(NL.Ctx { NL.lang=NL.English }) Unknown = "UNKNOWN"


-- ====================================================================== MONEY
  
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
  currentTime <- Clock.getCurrentTime
  let currentDay = Clock.utctDay currentTime
  putStrLn $ "as of " ++ Calendar.showGregorian currentDay
  putStrLn $ unlines $ [ NL.to
                         (NL.Ctx { NL.lang = natlang
                                 , NL.refTime = currentDay
                                 , NL.tense = Nothing
                                 })
                         round
                         | natlang <- [NL.English]
                         , round   <- myDefaults ]
--  putStrLn $ show myDefaults
  
