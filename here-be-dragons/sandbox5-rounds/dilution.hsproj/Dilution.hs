{-# LANGUAGE GADTs, StandaloneDeriving #-}

{-
the whole point of this exercise is to compare two scenarios, for the sake of showing investors how they dilute
over multiple rounds.
we could do this in a spreadsheet. in fact we will do it in a spreadsheet as well. but we want to eat our own v3 dogfood.
so we do it in our language.

in our case, we are interested in comparing a situation where a tranche 1 instrument converts prior to tranche 2, vs where instruments of both tranches convert at the same time. we want to calculate the effective price per share for someone who participated
in both tranches.

INITIAL NARRATIVE
just to get our feet wet we mess about with a fictional myCo that exercises all the operators in our language.

GOAL NARRATIVE
in the initial round there is one shareholder with one share.
in the split round that one share turns into 1 million shares. no money changes hands. this is a stock split.
in the esop round the company issues 3 million shares to somebody.
then investors come. the angel round has two tranches.
tranche 1 happens at a given cap, no discount, of $3.5M.
tranche 2 happens at a higher cap, no discount, of $6M.
suppose a seed round happens, issuing Series Seed preferred shares in a priced round.
the angel round safes convert at that time.
what is each investor's holding worth?

PHILOSOPHY
a company is the sum of its contracts
specialized to an investment perspective, a company is the sum of its investment agreements
which are collected into rounds
so a round is a set of investment agreements between investors and the company
but an investment agreement may be convertible! how excite.

before any given round, it is possible to summarize the state of a company, by folding over its rounds.
the summary is to the rounds as a balance sheet is to the accounting ledger of transactions: it is a snapshot.
-}


import Data.Monoid (Sum(Sum,getSum))
import Control.Monad
import qualified Data.Time as DT
import Data.Maybe (fromMaybe)
import qualified Control.Foldl as L
import qualified Data.Map as M
import Data.Function


-- a CapTable is mostly an ordered list of Rounds.

data CapTable = CapTable {
      companyName :: String
    , rounds :: [Round]
    }

data Round = Round {
      roundName :: String
    -- at the time of a given round, the company negotiates with its investors a pre-money valuation
    , preMoneyValuation :: Maybe Int
    -- a round issues one or more instruments, which bear a sort of prototype family relationship to one another.
    , holdings :: [Holding]
    }



-- data Holding = Holding {
--       amount :: 
--     , holder :: Party
--     , instrument :: Instrument
--     }


-- let y = x ++ succ(x)
--           in y ++ reverse y
             
data Instrument = Instrument {
      iname :: String
    , instrument' :: Instrument'
    } deriving (Eq, Ord)

data Instrument' = Ordinary
                 | Preferred
                 | Safe
                 | Debt { maturityDate :: DT.Day }
                 | Cash
                 | Convertible {
                     -- need some kind of refinement? type limiting the instrument'' to Debt / Preferred / Safe
                     instrument''       :: Instrument'
                   , cap                :: Maybe MoMoney
                   , discount           :: Maybe Rational
                   , conversionDiscount :: Maybe Rational
                   , maturityDiscount   :: Maybe Rational
                   , isQualifyingRound  :: Predicate
                   } deriving (Eq, Ord)

newtype MoMoney = MoMoney { momoney :: Int }
    deriving (Show, Eq, Ord)

data HoldingUnits = Money MoMoney | ShareUnits Int
                  deriving (Show, Eq, Ord)

hu (Money      x) = momoney x
hu (ShareUnits x) = x

leadHolder :: Round -> String
leadHolder r = holdings r & head & holder

-- we take the first instrument in the list to be the lead instrument, whose properties are inherited by all of, or most of, the other instruments in the list.
leadInstrument :: Round -> Instrument
leadInstrument r = holdings r & head & instrument

-- the amount raised in a given round is the sum of the amounts of all its instruments
roundRaised :: Round -> Int
roundRaised = getSum . foldMap (Sum . hu . amount) . holdings

-- the amount raised by a company across all its rounds
ctRaised :: CapTable -> Int
ctRaised = getSum . foldMap (Sum . roundRaised) . rounds

type Party = String

-- when tallying the instruments across a cap table, we want to credit (Cr.) and debit (Dr.) quantities of instruments over rounds.
-- for example, if alice gets 100 preferred A shares, then gets 20 preferred B shares, and later sells 70 A shares, how many preferred shares does alice have left? She has 30 A shares and 20 B shares.
-- to answer this question algorithmically, we need to know that the 70 A shares are different to the 20 B shares.
-- so, how do we know that an instrument in one round is the same as an instrument in another round?
-- first idea: we could stringify instruments using Show.
-- unfortunately, for now, Instrument contains functions.
-- if Haskell could Show a function, it wouldn't be Haskell any more; it would be lisp.
-- one solution: conceive of a "signature", where if two signatures are the same then they can be ledgered against one another.
-- another solution: create a Eq-uable, Show-able DSL to represent even the moving parts inside instruments.
-- for now, make Instrument an instance of Eq so we can compare them.

instance Eq Holding where
    a == b = holder     a == holder     b &&
             instrument a == instrument b

-- we can haz phantom types
data Foo
data Bar

data Inst a = InstStuff {moneys :: [Unit a]}

data Unit a = Unit { getUnit :: Int }

someInst :: Inst Foo
someInst = InstStuff [Unit 3, Unit 4]

someOtherInst :: Inst Bar
someOtherInst = InstStuff [Unit 5, Unit 1]

appender :: Inst a -> Inst a -> Inst a
appender (InstStuff x) (InstStuff y) = InstStuff (x ++ y)            

-- This shouldn't typecheck
phi = appender someInst someOtherInst


    

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
    LeadInstrument :: Query Instrument'

deriving instance Eq (Query a)
deriving instance Ord (Query a)

data Predicate where
  And :: Predicate -> Predicate -> Predicate
  Or :: Predicate -> Predicate -> Predicate
  Not :: Predicate -> Predicate
  IsEquity :: Query Instrument' -> Predicate

deriving instance Eq Predicate
deriving instance Ord Predicate

mkQuery :: Query a -> (Round -> a)
mkQuery RaisingAmount = roundRaised
mkQuery PremoneyEvaluation = fromMaybe 0 . preMoneyValuation
mkQuery PricePerShare = undefined -- the premoney divided by the number of shares in the company prior to the round, which comes from the snapshot
mkQuery LeadInstrument = instrument' . leadInstrument

mkPredicate :: Predicate -> (Round -> Bool)
mkPredicate (And x y) r = mkPredicate x r && mkPredicate y r
mkPredicate (Or x y) r = mkPredicate x r || mkPredicate y r
mkPredicate (Not x) r = not (mkPredicate x r)
mkPredicate (IsEquity q) r = isEquity (mkQuery q r)

-- we represent the world as a time plus the state of the captable
data World = World (CapTable, DT.Day)

-- ================================================================================ prettyprint
class MyShow x where
    myShow :: x -> String

instance MyShow CapTable where
    myShow ct = "* Cap Table for company " ++ (companyName ct) ++ "\n" ++
                (concat $ map myShow (rounds ct))

instance MyShow Round where
    myShow round = "** Round " ++ (roundName round) ++ "\n" ++
                   (concat $ map myShow (holdings round))

instance MyShow Holding where
    myShow holding = "*** Holder " ++ holder holding ++
                     " has " ++ show (amount holding) ++ " " ++ (iname (instrument holding)) ++ "\n"


-- ================================================================================ HOLDINGS AND FOLDINGS



-- fold a captable ledger to a summary of current holdings
-- see http://www.haskellforall.com/2013/08/composable-streaming-folds.html
-- the running tally monoid is a Data.Map of holding name/instrument to amount
-- so that the summarize of a myCo3 should be a Round = [Holding] with, inter alia,
--   (Holding 3000 Alice basicdebt),
--   (Holding 1500 Alice ordinary)
-- snapshot :: L.Fold Round Round
-- snapshot = L.Fold (\round -> undefined) -- each holder in the round -> map of (name/instrument , amount)
--                 (\__mymap__ -> undefined {- some round -})

damnitGabriel :: Monoid m => (a -> m) -> (m -> b) -> L.Fold a b
damnitGabriel tally summarize = L.Fold (\old new -> mappend old (tally new)) mempty summarize

snapshot :: L.Fold Round Round
snapshot = damnitGabriel
           (\round -> HolderTracker undefined)
-- M.fromList $ map (\h -> ((holder h, instrument h), amount h)) (holdings round)) )
           (\(HolderTracker ht) -> (Round "snapshot" Nothing (M.elems (M.mapWithKey (\(hp, hi) ha -> Holding ha hp hi) ht))))

newtype HolderTracker = HolderTracker (M.Map (Party, Instrument) HoldingUnits)

instance Monoid HolderTracker where
    mempty = HolderTracker M.empty
    mappend (HolderTracker a) (HolderTracker b) = HolderTracker (M.unionWith appendHoldingUnits a b)

appendHoldingUnits :: HoldingUnits -> HoldingUnits -> HoldingUnits
appendHoldingUnits (Money (MoMoney x)) (Money (MoMoney y)) = eff (x + y)
appendHoldingUnits (Money x) (ShareUnits y) = error "you still stupid"
appendHoldingUnits (ShareUnits x) (Money y) = error "you stupid"
appendHoldingUnits (ShareUnits x) (ShareUnits y) = ShareUnits (x + y)



-- ================================================================================ INITIAL EXERCISE

{-
Round round get around
I get around
Yeah
Get around round round I get around
I'm makin' real good bread

-- Beach Boys
-}


-- ---------------------------------------- ROUND

-- let's set up a dummy company that has just one round -- incorporation.
myCo :: CapTable
myCo = (CapTable "My First Startup"
        [Round "Incorporation" (Just 0) [(Holding (ShareUnits 1000) "Alice" ordinary)
                                        ,(Holding (ShareUnits 2000) "Bob"   ordinary)]])
ordinary = (Instrument "Ordinary Shares" Ordinary)

-- ---------------------------------------- ROUND

eff = Money . MoMoney

-- yay! the company has raised an angel round.
myCo2 = addRound myCo (Round "Angel" Nothing [(Holding (eff 3000) "Carol" basicdebt)
                                             ,(Holding (eff 4000) "Alice" basicdebt)
                                             ])
basicdebt = (Instrument "Basic Debt" (Debt (DT.fromGregorian 2018 6 1)))

addRound :: CapTable -> Round -> CapTable
-- we go through every existing instrument, announcing the new round, and let it handle that event
addRound oldCT curRound =
    -- any converting instruments from the oldCT, should show up in the newRound
    -- as  a + of the post-conversion instrument
    -- and a - of the  pre-conversion instrument
    -- of course, we only consider instruments which exist in the current snapshot
    let conversions = []
        newRound = curRound { holdings = concat [conversions, holdings curRound] }
    in oldCT { rounds = newRound : (rounds oldCT) }


-- ---------------------------------------- ROUND

-- the company reshuffles some shares
myCo3 = addRound myCo2 (Round "Reshuffle" Nothing [(Holding (ShareUnits (-1000)) "Alice" basicdebt)
                                                  ,(Holding (ShareUnits    500)  "Alice" ordinary)
                                                  ,(Holding (ShareUnits    200)  "Bob" ordinary)])

-- ---------------------------------------- ROUND
-- the company issues a convertible round with certain conditions for conversion.
-- the conditions are: if the round is equity, it converts. if it isn't, it doesn't.

isEquity :: Instrument' -> Bool
isEquity Ordinary        = True
isEquity Preferred       = True
isEquity (Convertible c _ _ _ _ _) = isWarrant c
isEquity _               = False

isDebt :: Instrument' -> Bool
isDebt (Debt _)          = True
isDebt (Convertible c _ _ _ _ _) = isDebt c
isDebt _                 = False

isWarrant :: Instrument' -> Bool
isWarrant Safe            = True
isWarrant (Convertible c _ _ _ _ _) = isWarrant c
isWarrant _               = False

round4note = Convertible { instrument'' = (Debt (DT.fromGregorian 2019 1 1))
                         , cap = Just (MoMoney 2000000)
                         , discount = Nothing
                         , conversionDiscount = Nothing
                         , maturityDiscount = Nothing
                         , isQualifyingRound = IsEquity LeadInstrument
                         }

myCo4 = addRound myCo3 (Round "Preseed Convertible" Nothing [(Holding (eff 10000)) "Carol"
                                                             (Instrument "Convertible Note"
                                                              round4note)])


-- ---------------------------------------- ROUND
-- the company issues some equity, which meets the qualifying conditions for conversion.
-- a conversion turns one instrument into another, subtracting the original instrument
