module Contract where

import qualified Data.Maybe as Maybe
import qualified Data.ISO3166_CountryCodes as Country
import qualified Data.Time.Clock    as Clock
import qualified NaturalLanguage    as NL -- most of the output related code lives here
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Time.Calendar as Calendar

type FixedDate = Calendar.Day
data DateExpr a = FixedDate FixedDate
                | DateFunc (a -> Calendar.Day)
instance Show (DateExpr a) where
  show (FixedDate day) = show day
  show (DateFunc df)   = show "a function returning Day"
                           
type PartyName = String
type PartyId   = String
type PartyAddress = String
type PartyEmail = String

data SimpleParty = SP { partyName    :: Maybe PartyName
                      , partyId      :: Maybe PartyId
                      , partyAddress :: Maybe PartyAddress
                      , partyEmail   :: Maybe [PartyEmail]
                      , jurisdiction :: Maybe Country.CountryCode
                      }
                 deriving (Show)

-- for trusts, etc
data ComplexParty = CP { beneficialOwner :: Maybe SimpleParty
                       , representative  :: Maybe SimpleParty }
                 deriving (Show)

data Party = NaturalPerson        SimpleParty
           | Corporation          SimpleParty
           | OtherSimple   String SimpleParty
           | ComplexParty         ComplexParty
                 deriving (Show)

data Contract = Contract { agreementDate :: Maybe FixedDate
                         , effectiveDate :: Maybe (DateExpr Contract)
                         , parties :: [Party]
                         }
                 deriving (Show)
