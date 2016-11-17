-- this is the Wrong Way to do things.
-- in Cambridge, MA 20161116
-- is the Wrong Path really wrong, if it is the only place from which one can glimpse the Right Path?

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADT #-}

module Main where

-- in which we develop a semantic for convertible notes, as seen in startup financings
-- requirements:
-- - be able to express most of the common forms of convertible notes out there -- cap, discount, MFN
-- - fold over a ledger of payments over time, because maybe the interest got paid out regularly
-- - compute the amount of principal and interest owed at a given time
-- - determine if a proposed financing is a qualifying event
-- - compute the conversion upon a qualifying financing -- cap and discount
-- - be able to output to GF, so GF can express the convertible notes in natural language

import Data.Ratio
import Data.Time

-- a convertible note is a debt instrument.
-- debt instruments usually define a principal, a term, and an interest rate.
-- the term and the interest rate are usually defined in terms of time
-- example term: "principal (and accrued, unpaid interest) shall be repaid in three years"
-- interest rate: "interest shall accrue at 2% per quarter, compounded"


-- it would be nice to reuse existing Haskell date/interval libraries
data Interval = Daily | Weekly | Monthly | Quarterly | Yearly
newtype NInterval = NInterval (Int, Interval) -- term: 3 years; or interest payable every 3 months

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
  , interestType   :: InterestType -- simple or compound
  }

data ConvertibleNote = ConvertibleNote Note Conversion

data Conversion = Conversion
  { maturityDiscount    :: Float
  , onMaturity          :: EventHandler Date
  , optionalByHolder    :: EventHandler (NoticeFrom Holder)
  , optionalByCompany   :: EventHandler (Resolution Directors)
  , price               :: EventHandler FinancingEvent
  , qualifyingFinancing :: FinancingEvent -> Bool
  }

data DatedEvent = DatedEvent Event Date

data Event a where
  MkDate       :: Date              -> Event Date
  MkNotice     :: Sender -> Content -> Event Notice
  MkReso       :: Content           -> Event Resolution
  MkFinancing  :: Financing         -> Event Financing

data Financing = Financing Investors AbstractInstrument
    
-- let x :: Date = _ in MkDate x :: Event Date
-- Event Date -> bar
-- (MkDate (x :: Date)

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
