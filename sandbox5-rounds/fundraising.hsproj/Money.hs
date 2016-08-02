module Money where

import Currency
import Currency.Rates
import qualified Data.ISO3166_CountryCodes as Country
import qualified NaturalLanguage    as NL -- most of the output related code lives here
import Text.Printf
import Data.List.Split
import Data.List
  
-- little helper functions

commafy x = h++t
    where
        sp = break (== '.') $ show x
        h = reverse $ intercalate "," $ chunksOf 3 $ reverse $ fst sp
        t = snd sp

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


