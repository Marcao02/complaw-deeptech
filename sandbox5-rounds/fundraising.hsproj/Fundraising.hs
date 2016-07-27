
import Currency
import Currency.Rates
import qualified Data.ISO3166_CountryCodes as Country
import Text.Printf
import Data.Time
import Data.List.Split
import Data.List
import qualified Data.Map as M

import System.Environment ( getArgs )
import System.Console.GetOpt as GetOpt
import Data.Maybe ( fromMaybe )
data Flag = Version
          | Psn   (Maybe String)
          | Years Integer
          | Start Integer
          | End   Integer
  deriving (Show)

options :: [OptDescr Flag]
options =
 [ Option ['V','?'] ["version"]         (NoArg Version)       "show version number"
 , Option ['y']     ["year","years"]    (ReqArg (Years . read) "YEAR")  "years"
 , Option ['s']     ["start"]           (ReqArg (Start . read) "START") "start valuation"
 , Option ['e']     ["end"]             (ReqArg (End   . read) "END")   "end valuation"
 , Option ['p']     ["psn_0_127007"]    (OptArg Psn "PSN")              "psn"
 ]

myOpts :: [String] -> IO ([Flag], [String])
myOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: projectgrowth [OPTION...]"

defaultValGrowth = ValGrowth { years=10
                         , startValuation =   4000000
                         , endValuation   = 5000000000
                         }


opts2valgrowth :: ([Flag],[String]) -> ValuationGrowth
opts2valgrowth (flags, otherargs) = foldl flag2valgrowth defaultValGrowth flags

flag2valgrowth :: ValuationGrowth -> Flag -> ValuationGrowth
flag2valgrowth vg (Years y) = vg { years=y }
flag2valgrowth vg (Start n) = vg { startValuation=n }
flag2valgrowth vg (End   n) = vg {   endValuation=n }
flag2valgrowth vg (Psn _)   = vg
flag2valgrowth vg (Version) = vg

digify x = h++t
    where
        sp = break (== '.') $ show x
        h = reverse $ intercalate "," $ chunksOf 3 $ reverse $ fst sp
        t = snd sp


data Locale = Locale { localeCountry :: String -- this should be Country
                     , localeLang :: String }

locale = Locale { localeCountry = "SG"
                , localeLang = "EN" }

data Round = Round { roundName :: String
                   , tranches :: [Tranche]
                   }

data Tranche = Tranche { trancheName :: String
                       , milestones :: [Milestone]
                       , fundraisingTarget :: (Money, Money)
                       , deadline :: Day
                       , securityTemplate :: Security
                       }

type Milestone = (String, Bool)

type NumDays = Int

data SecurityNature = Ordinary | Preferred String | Debt
instance Show SecurityNature where
  show Ordinary = "Ordinary Shares"

data Security = Security { preMoney :: Maybe Money
                         , valuationCap :: Maybe Money
                         , discount :: Maybe Int -- numerator /100
                         , term :: NumDays -- change this to a dateinterval
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

rounds = [ Round { roundName="angel"
                 , tranches = [] } ]


-- how much does our valuation need to grow every year to hit our goal?
data ValuationGrowth = ValGrowth { years :: Integer
                                 , startValuation :: Integer
                                 , endValuation :: Integer
                                 }
                                 
instance Show ValuationGrowth where
  show vg = "If initial valuation is " ++ digify (startValuation vg) ++ " and want to get to " ++ digify (endValuation vg) ++ " in " ++ show (years vg) ++ " years,\n" ++
            "then every year our valuation will need to increase by " ++ show (yearlyGrowth vg) ++ " times.\n"
            
yearlyGrowth :: ValuationGrowth -> Float
yearlyGrowth (ValGrowth { years         =y
                        , startValuation=sv
                        , endValuation  =ev
                        }) =
                        exp ((log (fromIntegral ev)
                             -log (fromIntegral sv))
                            /      fromIntegral y)


-- formula: endValuation = startValuation * (yearlyGrowth ** years)
--        log(endValuation) = log(startValuation) + years * log(yearlyGrowth)
--       (log(endValuation) - log(startValuation))/ years = log(yearlyGrowth)
-- e  **((log(endValuation) - log(startValuation))/ years)=     yearlyGrowth


-- 10^((log(1e10)-log(4e6))/10

project valgrowth = do
  putStrLn $ show valgrowth
  putStrLn $ unlines $
     Data.List.map (\a -> printf "in %d, we will be worth %14s"
           ((2016+a)::Int) -- printf gets snippy without the explicit type
           (digify $ truncate (fromIntegral
            (startValuation valgrowth) *
             (yearlyGrowth valgrowth)
              ** (fromIntegral a)))
           )
     (take ((1+) $ fromIntegral $ years valgrowth) [0..])


main = do
  putStrLn "hello, world!"
  myargs <- getArgs
  myopts <- myOpts myargs
  project $ opts2valgrowth myopts
 













