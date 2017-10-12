
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

-- emacs haskell-mode with intero
-- C-c ! n for next flycheck error
-- C-c ! p for prev flycheck error

module Company where

import Data.Aeson.Diff
import Data.Aeson
import Data.Time
import GHC.Generics
import Control.Applicative
import Control.Monad
import qualified Data.Text as T (unpack)

data CompanyState = CompanyState { parties    :: [Party]
                                 , securities :: [Security]
                                 , company    ::  Company
                                 , holdings   :: [Holding]
                                 , agreements :: [Contract]
                                 } deriving (Generic, ToJSON, FromJSON, Show, Eq)

data Party = Party { fullname :: PartyName
                   , idtype   :: String
                   , idnum    :: String
                   , nature   :: EntityNature
                   , gender   :: Gender
                   } deriving (Generic, ToJSON, FromJSON, Show, Eq)
             
data Company = Company { name :: String
                       , jurisdiction :: String -- change to some ISO code
                       , address :: String
                       , idtype :: String
                       , idnum  :: String
                       , constitution :: Constitution
                       }
             deriving (Show, Eq, Generic, ToJSON, FromJSON)

type Constitution = [ConExp]
data                 ConExp = ConExp { title :: String
                                     , body  :: String }
                            deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Security = Security { name :: String
                         , measure :: Measure
                         } deriving (Generic, ToJSON, FromJSON, Show, Eq)
                
data Holding = Holding { holder :: PartyName
                       , holds  :: [HeldSecurity]
                       } deriving (Generic, ToJSON, FromJSON, Show, Eq)

type PartyName = String

data Contract = Contract { parties :: [PartyName]
                         , dated   :: Day
                         , title   :: String
                         , singleton :: Bool
                         } deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- Prelude Data.Aeson Data.Time> decode (encode $ toJSON $ fromGregorian 1970 1 2) :: Maybe Day
-- Just 1970-01-02

data HeldSecurity = HeldSecurity { securityName :: String
                                 , units :: Maybe Float
                                 , money :: Maybe Float
                                 , description :: Maybe String
                                 } deriving (Generic, ToJSON, FromJSON, Show, Eq)
-- we need some way to sanity-check the input that either units xor money is given

data Measure     = ByUnit | ByMoney | OtherMeasure String deriving (Show, Eq, Generic, ToJSON)
instance FromJSON Measure where
  parseJSON = withText "measure" $ \v -> return $ case v of
    "byunit"  ->  ByUnit
    "bymoney" ->  ByMoney
    _         ->  OtherMeasure (T.unpack v)

data Measurement = Units Int | Money Currency deriving (Show, Eq)
                      
data EntityNature = Human | Corporate | AI | OtherNature String
                  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON EntityNature where
  parseJSON = withText "nature" $ \v -> return $ case v of
    "human"     ->  Human
    "corporate" ->  Corporate
    "AI"        ->  AI
    _        ->  OtherNature (T.unpack v)

data Gender = Female | Male | Neutral | OtherGender String
            deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Gender where
  parseJSON = withText "gender" $ \v -> return $ case v of
    "female" ->  Female
    "male"   ->  Male
    "neutral" -> Neutral
    _        ->  OtherGender (T.unpack v)

-- TODO: use some currency library
data Currency = Currency { code  :: CurrencyCode
                         , cents :: Int }
                deriving (Show, Eq)

newtype CurrencyCode = CurrencyCode String deriving (Show, Eq)

  

