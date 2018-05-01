-- my first haskell program!
{-# LANGUAGE ViewPatterns #-}
module Legalese where

import Currency
import qualified Data.ISO3166_CountryCodes as Country
import Data.String
import Data.Char
-- import GenI



class (RealFloat a) => Feq a where
  (==~) :: a -> a -> Bool

-- 1.00000000001 ==~ 1.0000000002 is True
instance Feq Double where
  a ==~ b = abs (a - b) < 1e-5



-- things happen in time
data Time = Int deriving (Show)

-- things happen in space
data Jurisdiction = Country

-- entities exist. both humans and corporations are called "persons".
data Person = Person String
              deriving (Show)

-- value changes hands in the form of money
data Money = Money { amount   :: Float -- in future use minorUnits
                   , currency :: Currency
                   } deriving (Show)


--instance IsString Currency where
--  fromString = easyCurrency

instance IsString Person where
  fromString = Person

-- map "SGD" to NationalCurrency Country.SG 'D'
{-
easyCurrency :: String -> Currency
easyCurrency (map toUpper -> "SGD") = NationalCurrency Country.SG 'D'
easyCurrency other              = error $ "Did not yet implement currency code for: " ++ show other
 -}

-- value changes hands in the form of corporate securities
data ShareType = Common
               deriving (Show)

data FinancialInstrument = Shares ShareType
                         | Bonds
                         deriving (Show)

-- persons can communicate.
data Message = Intend   [EventSpec]
             | Happened [EventSpec]
             | Invite Statement
               deriving (Show)

-- when things happen in time, they are events.
data Event = Ev { when :: Time
                , what :: EventType }
           deriving (Show)

-- usually, an actor is responsible for an event
data EventSpec = Event Person EventType
               deriving (Show)

data EventType = Pay      { rcpt :: Person, money :: Money }
               | Send     { rcpt :: Person, msg :: Message }
               | Resolution Message
               | Issue    { rcpt     :: Person
                          , security :: FinancialInstrument
                          , quantity :: Integer
                          }
                 deriving (Show)

-- we now have enough primitives to make some statements in our language.






data Statement = -- First order logic
                 Statement :&&: Statement -- and
               | Statement :||: Statement -- or
               | Statement :=>: Statement -- implication
               | Not Statement            -- negation
               | StTrue                   -- Always true
                 -- Deontic
               | May      Person Statement
               | Must     Person Statement
               | Happens  [EventSpec]
                 -- Temporal modalities
               | At     Time Statement
               | During Time Time Statement
               | Prior  Time Statement
               | Post   Time Statement
                 -- Epistemic
               | Believes Person Statement
               | Knows    Person Statement
                 -- Definitions
                 -- TODO: different types of definitions
               | Means    Person Person
                 deriving (Show)

isSt :: Statement -> Bool
isSt (a :&&: b) = isSt a && isSt b
isSt (a :||: b) = isSt a || isSt b
isSt (Not    a) = not $ isSt a
isSt  StTrue    = True

type EventStream = [Event]

check :: EventStream -> Statement -> Bool
check = undefined

proceed :: EventStream -> Person -> [Event]
proceed  = undefined

parseStory :: String -> EventStream
parseStory  = undefined

data Language = EN_SG
              | Ethereum

generateContract :: Statement -> Language -> String
generateContract  = undefined

-- outputs
  
-- at the specification level, Statement -> to natural language
asSpec :: Statement -> String
asSpec st = show st

-- at the implementation level, Statement -> [EvStream] -> [EvStream]
-- given the history so far (some of which may include a proposed scenario)
-- what actions should the parties take next?


-- at the validation level, Statement -> [EvStream] -> [ContractState]
-- who has breached the contract?


