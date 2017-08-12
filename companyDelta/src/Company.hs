
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
import GHC.Generics
import Control.Applicative
import Control.Monad
import qualified Data.Text as T (unpack)

data CompanyState = CompanyState { holders :: [Holder]
                                 , securities :: [Security]
                                 , company :: Company
                                 , holdings :: [Holding]
                                 } deriving (Generic, ToJSON, Show, Eq)

data Company = Company { name :: String
                         , jurisdiction :: String -- change to some ISO code
                         , idtype :: String
                         , idnum  :: String
                         }
             deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Holder = Holder { fullname :: String
                     , idtype   :: String
                     , idnum    :: String
                     , nature   :: String
                     , gender   :: Gender
                     } deriving (Generic, ToJSON, FromJSON, Show, Eq)

data Security = Security { name :: String
                                 , measure :: String
                                 } deriving (Generic, ToJSON, FromJSON, Show, Eq)

data Holding = Holding { holder :: String
                               , holds  :: [HeldSecurity]
                               } deriving (Generic, ToJSON, FromJSON, Show, Eq)

data HeldSecurity = HeldSecurity { securityName :: String
                                 , units :: Maybe Float
                                 , money :: Maybe Float
                                 } deriving (Generic, ToJSON, FromJSON, Show, Eq)
-- compute the CRUD deltas between two snapshots of company state


data Measure     = ByUnit    | ByMoney        deriving (Show, Eq)
data Measurement = Units Int | Money Currency deriving (Show, Eq)
                      
                             
data EntityNature = Human | Corporate | AI
                  deriving (Show, Eq)

data Gender = Female | Male | Neutral | OtherGender String
            deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Gender where
  parseJSON = withText "gender" $ \v -> return $ case v of
    "female" ->  Female
    "male"   ->  Male
    _        ->  OtherGender (T.unpack v)

data Agreement = Agreement deriving (Show, Eq)

-- TODO: use some currency library
data Currency = Currency { code  :: CurrencyCode
                         , cents :: Int }
                deriving (Show, Eq)

newtype CurrencyCode = CurrencyCode String deriving (Show, Eq)

--
-- set up some sample data
--

potato = "Potato"

--
-- compare two companystates
-- 






