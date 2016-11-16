-- this is the Wrong Way to do things.
-- in Cambridge, MA 20161116
-- is the Wrong Path really wrong, if it is the only place from which one can glimpse the Right Path?

{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Ratio
import Data.Time

data Interval = Daily | Weekly | Monthly | Quarterly | Yearly
newtype NInterval = NInterval (Int, Interval)

type SphericalCow = () -- cow says "nooooolll"

interestPeriods :: (Gregorian, Gregorian) -> Interval -> Int
interestPeriods (n, m) Daily = undefined
interestPeriods (n, m) Weekly = undefined
interestPeriods (n, m) Monthly = undefined
interestPeriods (n, m) Quarterly = undefined
interestPeriods (Gregorian y0 m0 d0, Gregorian y1 m1 d1) Yearly =
  fromIntegral (y1 - y0)


-- Date -> (Interest, Principal)

data Gregorian = Gregorian Integer Int Int

data InterestType = SimpleInterest | CompoundInterest

toGregorian' :: Day -> Gregorian
toGregorian' day = let (yyyy, mm, dd) = toGregorian day in Gregorian yyyy mm dd

data Note = Debt
  { principal      :: Float     -- in future this becomes an actual currency with x00 units -- see ISO4217Currency
  , interestRate   :: Float     -- 7% interest = 0.07
  , interestPeriod :: NInterval -- interest shall be charged at 5% every two quarters = interestPeriod 2 Quarterly
  , startDate      :: Gregorian
  , maturityDate   :: Gregorian
  , interestType   :: InterestType
  }

data ConvertibleNote = ConvertibleNote Note Conversion

data Conversion = Conversion
  { maturityDiscount  :: Float
  , onMaturity        :: EventHandler
  , optionalByHolder  :: EventHandler
  , optionalByCompany :: EventHandler
  }

data EventHandler = Event -> Note -> Note
  
someNote :: Note
someNote = Debt 20 (0.10) (NInterval (1, Yearly)) (Gregorian 1970 01 01) (Gregorian 1972 01 01) SimpleInterest

-- data ConcreteNote = ConcreteNote Note NoteLog

roundsOfInterest :: Note -> Int
roundsOfInterest Debt{..} =  let NInterval (n, t) = interestPeriod in n * interestPeriods (startDate, maturityDate) t



automaticConversionUnderAssumptionOfNoInterveningActivity :: Note -> Float
automaticConversionUnderAssumptionOfNoInterveningActivity d@Debt{..} = case interestType of
  SimpleInterest   -> principal * (1 + (interestRate * (fromIntegral $ roundsOfInterest d)))
  CompoundInterest -> foldr (*) principal $ replicate (roundsOfInterest d) (1 + interestRate)



-- automaticConversion :: Note -> Int
-- automaticConversion Debt{..} = _


main = return ()
