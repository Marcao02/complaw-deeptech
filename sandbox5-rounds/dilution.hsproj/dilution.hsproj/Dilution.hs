{-# LANGUAGE GADTs #-}

-- the whole point of this exercise is to compare two scenarios, for the sake of responding to an email from Walden.
-- in our case, we are interested in comparing a situation where a tranche 1 instrument converts prior to tranche 2, vs where instruments of both tranches convert at the same time.

-- PHILOSOPHY
-- a company is the sum of its contracts
-- specialized to an investment perspective, a company is the sum of its investment agreements
-- which are collected into rounds
-- so a round is a set of investment agreements between investors and the company
-- but an investment agreement may be convertible! how excite.

import Data.Monoid (Sum(Sum,getSum))
import Control.Monad
import qualified Data.Time as DT

-- a CapTable is an ordered list of Rounds.

type CapTable = [Round]

data Round = Round {
      name :: String
    -- at the time of a given round, the company negotiates with its investors a pre-money valuation
    , preMoneyValuation :: Maybe Int
    -- a round issues one or more instruments, which bear a sort of prototype family relationship to one another.
    , instruments :: [Instrument]
    }

-- we take the first instrument in the list to be the lead instrument, whose properties are inherited by all of, or most of, the other instruments in the list.
leadInstrument :: Round -> Instrument
leadInstrument = head . instruments

-- the amount raised in a given round is the sum of the amounts of all its instruments
roundRaised :: Round -> Int
roundRaised = getSum . foldMap (Sum . amount) . instruments

-- the amount raised by a company across all its rounds
ctRaised :: CapTable -> Int
ctRaised = getSum . foldMap (Sum . roundRaised)
              
data Instrument = Instrument {
      amount :: Int
    , holder :: Party
    , instrument :: Instrument'
    -- event handlers below
    , conversion  :: Maybe () -- Conversion -- (World -> Round -> Maybe Instrument)
    }
data Instrument' = Ordinary
                 | Preferred
                 | Safe
                 | Debt { maturityDate :: DT.Day }
                 | Cash

type Party = String



    
-- let's do a tiny DSL for conversion so we can compare them.
-- - is the new round a qualifying round? if not, we don't convert.
-- - a new round can qualify based on the amount it is raising; or the pre-money valuation.
-- - what's the price per share of the new round? that will tell us how many shares we convert to.
-- - the converting instrument may specify if it wants to convert to the newly issued instrument, or to common stock.
-- - the converting instrument may specify a discount, in which case the price per share of the new round will be discounted and the holder will get correspondingly more shares.
-- - the converting instrument may specify a cap: if the price per share of the new round, even after discount, exceeds the cap, then we just use the price per share from the cap, instead.

-- data Conversion = Conversion IsQualifying

data Query a where
    RaisingAmount :: Query Int
    PremoneyEvaluation :: Query Int
    PricePerShare :: Query Int

data Predicate where
  And :: Predicate -> Predicate -> Predicate
  Or :: Predicate -> Predicate -> Predicate
  Not :: Predicate -> Predicate
  GreaterThan :: Ord a => Query a -> a -> Predicate
  LessThan :: Ord a => Query a -> a -> Predicate
  EqualTo :: Eq a => Query a -> a -> Predicate

somePredicate :: Predicate
somePredicate = (GreaterThan RaisingAmount 300) `And` (LessThan PricePerShare 20)

fromMaybe :: a -> (Maybe a -> a)
fromMaybe _ (Just k) = k ; fromMaybe e Nothing  = e

mkQuery :: Query a -> (Round -> a)
mkQuery RaisingAmount = roundRaised
mkQuery PremoneyEvaluation = fromMaybe 0 . preMoneyValuation
mkQuery PricePerShare = undefined -- the premoney divided by the number of shares in the company prior to the round, which comes from the snapshot

mkPredicate :: Predicate -> (Round -> Bool)
mkPredicate (And x y) r = mkPredicate x r && mkPredicate y r
mkPredicate (Or x y) r = mkPredicate x r || mkPredicate y r
mkPredicate (Not x) r = not (mkPredicate x r)
mkPredicate (GreaterThan q n) r = mkQuery q r > n
mkPredicate (LessThan q n) r = mkQuery q r < n
mkPredicate (EqualTo q e) r = mkQuery q r == e


                              
snapshot = undefined    
    
-- we represent the world as a time plus the state of the captable
data World = World (CapTable, DT.Day)

-- let's set up a dummy company that has just one round -- incorporation.
myCo :: CapTable
myCo = [Round "Incorporation" (Just 0) [(Instrument 1000 "Alice" Ordinary Nothing)
                                       ,(Instrument 2000 "Bob"   Ordinary Nothing)]]

-- yay! the company has raised an angel round.
myCo2 = addRound myCo (Round "Angel" Nothing [(Instrument 3000 "Carol" (Debt (DT.fromGregorian 2018 6 1)) Nothing)])
                                                                                         

addRound :: CapTable -> Round -> CapTable
-- we go through every existing instrument, announcing the new round, and let it handle that event
addRound oldCT curRound =
    -- any converting instruments from the oldCT, should show up in the newRound
    -- as  a + of the post-conversion instrument
    -- and a - of the  pre-conversion instrument
    -- of course, we only consider instruments which exist in the current snapshot
    let conversions = flip map (snapshot oldCT) $ \i -> i { instrument = Cash }
        newRound = curRound { instruments = concat [conversions, instruments curRound] }
    in newRound : oldCT

-- fold a captable ledger to a summary of current holdings
-- see http://www.haskellforall.com/2013/08/composable-streaming-folds.html
-- Round 1: [1000 Alice Ordinary]
-- Round 2: [-250 Alice Ordinary]
-- should produce the snapshot [750 Alice Ordinary] which we can then evaluate for conversion.
-- snapshot :: CapTable -> [Instrument]           
-- snapshot ct = 
              
-- isPrimary :: [Instrument] -> (Instrument -> Bool)

-- when an instrument converts, it does so with a given price per share, determined by the pre-money valuation cap and the number of fully diluted shares in the company prior to conversion.

-- class Instrument a where
--     liquidationPreference :: a  -> Maybe Int
--     liquidationPreference = const Nothing
--     conversion :: a -> Round -> b
                             
-- data Jargon = forall a. Instrument a => Jargon a

-- instance Instrument Preferred where
--     liquidationPreference = pliquidation_preference

-- instance Instrument (Ordinary a) where {}
-- instance Instrument Safe where {}

-- instance Instrument Jargon where
--     liquidationPreference (Jargon k) = liquidationPreference k

-- data Ordinary a = Ordinary
--     { oholderName :: String
--     , opricePerShare :: Maybe Int
--     , oconversion :: a
--     }

-- data Safe = Safe
--     { sholderName :: String
--     , sdiscount :: Maybe Int
--     }

-- data Preferred = Preferred
--     { pholderName :: String
--     , pliquidation_preference :: Maybe Int
--     }


-- class Convertible a b where

-- instance Convertible a a where
--     conversion = const

-- instance Convertible (Ordinary a) Safe where
--     conversion Ordinary{..} Round{..} = Safe
--                                         { sholderName = oholderName
--                                         , sdiscount = Nothing
--                                         }

-- instance Convertible Safe Preferred where
--     conversion Safe{..} Round{..} = Preferred
--                                         { pholderName = sholderName
--                                         , pliquidation_preference = liquidationPreference leadInstrument 
--                                         }



-- -- class Convertible a 
-- --  where
-- --    conversion :: Instrument -> Round -> Instrument




-- -- a conversion takes:
-- -- - the history of a company, as represented by its CapTable,
-- -- - the current round that is now happening
-- -- - an output captable containing the new round, with the post-conversion instruments also represented
-- -- roundConversion :: CapTable -> Round -> CapTable
-- -- roundConversion origCT newRound =
-- --   -- 



     
