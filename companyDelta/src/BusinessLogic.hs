{-# LANGUAGE DuplicateRecordFields #-}

module BusinessLogic where

-- for the company data model
import Company
import CDiff

-- graph
import Data.Graph

-- what paperwork is required to effect a change?

data DirectorsResolution = DR { title :: String
                              , body  :: String
                              , id    :: Int
                              } deriving (Show)
                           
data MembersResolution = MR { title :: String
                            , body  :: String
                            , id    :: Int
                            , level :: MRlevel
                            } deriving (Show)

data Agreement = Ag { title :: String
                    , body  :: String
                    , id    :: Int
                    } deriving (Show)

data MRlevel = Ordinary | Special
             deriving (Show)

data Paperwork = Paperwork { dr :: [DirectorsResolution]
                           , mr :: [MembersResolution]
                           , ag :: [Agreement]
                           }
               deriving (Show)

data Temporal = Pre | Post DurationSpec | Simul
              deriving (Show)

data DurationSpec = DurationYMD Int Int Int
                  deriving (Show)

data Rule = RuleDiff      Temporal Diff      Paperwork
          | RulePaper     Temporal Paperwork Paperwork
-- RuleDiff Pre   X Y means: before we can do X, we must first do Y
-- RuleDiff Post  X Y means:  after we     do X, we must next  do Y by DurationSpec, as a relative deadline
-- RuleDiff Simul X Y means:   when we     do X, we must simultaneously do Y

{- changes to CompanyState -}

{- changes to the Company -}

-- change of address requires directors resolution
-- and requires notification to all parties to all agreements

{- changes to Holders -}

{- changes to a particular Holder -}

{- changes to Securities -}

{- changes to a particular Security -}

{- changes to the Holder/Security relation -}

{- changes to a particular Security -}




