
module ProjectValuation where
  
import Data.List.Split
import Data.List
import qualified Data.Map as M
import Text.Printf
import Security
import Money
import qualified Data.Time.Clock    as Clock
import qualified Data.Time.Calendar as Calendar

import System.Environment ( getArgs )
import System.Console.GetOpt as GetOpt
import Data.Maybe ( fromMaybe )
data Flag = Version
          | Psn   (Maybe String)
          | Years Integer
          | Firstyear Integer
          | Start Integer
          | End   Integer
  deriving (Show)

options :: [OptDescr Flag]
options =
 [ Option ['V','?'] ["version"]         (NoArg Version)       "show version number"
 , Option ['f']     ["firstyear"]       (ReqArg (Firstyear . read) "FIRST")  "first year"
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

opts2valgrowth :: ([Flag],[String]) -> ValuationGrowth -> ValuationGrowth
opts2valgrowth (flags, otherargs) defaultVG = foldl flag2valgrowth defaultVG flags

flag2valgrowth :: ValuationGrowth -> Flag -> ValuationGrowth
flag2valgrowth vg (Years y) = vg { years=y }
flag2valgrowth vg (Firstyear y) = vg { firstYear=y }
flag2valgrowth vg (Start n) = vg { startValuation=n }
flag2valgrowth vg (End   n) = vg {   endValuation=n }
flag2valgrowth vg (Psn _)   = vg
flag2valgrowth vg (Version) = vg


-- how much does our valuation need to grow every year to hit our goal?
data ValuationGrowth = ValGrowth { years :: Integer
                                 , firstYear :: Integer
                                 , startValuation :: Integer
                                 , endValuation :: Integer
                                 }
                                 
instance Show ValuationGrowth where
  show vg = "If initial valuation is " ++ commafy (startValuation vg) ++ " and want to get to " ++ commafy (endValuation vg) ++ " in " ++ show (years vg) ++ " years,\n" ++
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
           (firstYear valgrowth + a)
           (commafy $ truncate (fromIntegral
            (startValuation valgrowth) *
             (yearlyGrowth valgrowth)
              ** (fromIntegral a)))
           )
     (take ((1+) $ fromIntegral $ years valgrowth) [0..])


main = do
  myargs <- getArgs
  myopts <- myOpts myargs
  (cY, cM, cD) <- fmap (Calendar.toGregorian . Clock.utctDay) Clock.getCurrentTime
  project $ opts2valgrowth myopts ValGrowth { years=10
                             , firstYear = cY
                             , startValuation =   4000000
                             , endValuation   = 5000000000
                         }


 













