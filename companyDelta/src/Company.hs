
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- emacs haskell-mode with intero
-- C-c ! n for next flycheck error
-- C-c ! p for prev flycheck error

module Company where

import Data.Aeson.Diff
import Data.Aeson

-- compute the CRUD deltas between two snapshots of company state

data CompanyState = MkCompanyState { company    :: Company
                                   , securities :: [Security]
                                   , holders    :: [Holder]
                                   , holdings   :: [Holding]
                                   , agreements :: [Agreement]
                                   }
                  deriving (Show, Eq)

data Company = MkCompany { name :: String
                         , jurisdiction :: String -- change to some ISO code
                         , idtype :: String
                         , idnum  :: String
                         }
             deriving (Show, Eq)

data Measure     = ByUnit    | ByMoney        deriving (Show, Eq)
data Measurement = Units Int | Money Currency deriving (Show, Eq)
                      
data Security = Security { name :: String,
                           measure :: Measure
                         }
              deriving (Show, Eq)
                       
data Holder = MkHolder { name :: String
                       , nature :: EntityNature
                       , gender :: Gender
                       , idtype :: String
                       , idnum  :: String
                       } deriving (Show, Eq)

data Holding = MkHolding { holder   :: Holder
                         , security :: Security
                         , quantity :: Measurement }
             deriving (Show, Eq)
                             
data EntityNature = Human | Corporate | AI
                  deriving (Show, Eq)

data Gender = Female | Male | Neutral | OtherGender String
            deriving (Show, Eq)

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






