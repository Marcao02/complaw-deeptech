{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}

module BusinessLogic where

-- for the company data model
import Company
import CDiff
import Control.Monad

-- figuring out the types at runtime for some things that we really shouldn't have to.
-- maybe we want Data.Data?
import Data.Typeable

-- graph
import Data.Graph
import Data.Tree

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
                    , level :: AgLevel
                    } deriving (Show)

data AgLevel = LOI | AgContract | Deed
             deriving (Show)

data Notice = Notice { title :: String
                     , body  :: String
                     , id    :: Int
                     } deriving (Show)

data MRlevel = Ordinary | Special
             deriving (Show)

data Paperwork = Paperwork { dr :: [DirectorsResolution]
                           , mr :: [MembersResolution]
                           , ag :: [Agreement]
                           , id :: Int
                           }
               deriving (Show)

data Temporal = Pre
              | Post DurationSpec
              | Simul
              deriving (Show)

data DurationSpec = DurationYMD Int Int Int
                  deriving (Show)

data Rule = RuleDiff      Temporal MatchDiff      Paperwork
          | RulePaper     Temporal MatchPaperwork Paperwork
-- RuleDiff Pre   X Y means: before we can do a thing that matches X, we must first do Y
-- RuleDiff Post  X Y means:  after we     do a thing that matches X, we must next  do Y by DurationSpec, as a relative deadline
-- RuleDiff Simul X Y means:   when we     do a thing that matches X, we must simultaneously do Y

type MatchDiff      = DiffBox -> Bool
type MatchPaperwork = Paperwork -> Bool

anyDiff :: MatchDiff
anyDiff = const True

{- changes to CompanyState -}

-- we recurse through the CompanyState,
-- pattern-matching Diff nodes against our Rule database;
-- every time a rule matches, we compute the paperwork required
-- to effect the diff.

-- we'll start this off with an unsorted list of paperwork.
-- later, we'll add a Graph which tracks the dependencies
-- and we'll write an Applicative instance which allows us to grow the Paperwork list and the Graph of dependencies
-- and we'll chuck the whole thing inside an RWST for maximum obfuscation
type Deps = [Paperwork]

ruleBase :: [Rule]
ruleBase = [ RuleDiff Pre shaChanged
             (Paperwork
              [ DR "company to circulate deed of ratification and accession" "resolved, that the company should circulate a deed of ratification and accession to the shareholders agreement" 0
              , DR "company to sign aforesaid deed" "resolved, that the company should ratify the aforesaid deed" 1
              ]
              [] [] 0) ]
  where shaChanged :: MatchDiff
        shaChanged (DiffBox (Update old new comment)) =
          typeOf new == typeOf Contract
          &&
          case cast new of
            Just wot -> (title :: Contract -> String) wot == "shareholdersAgreement"
            Nothing -> False
        shaChanged _ = False
        -- jeebus, do i really need https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Typeable.html ?
        isSha :: Contract -> Bool
        isSha contract = (title (contract :: Contract) == "shareholdersAgreement")


 --                 { rootLabel =
 --                     "changed Contract"
 --                     Update
 --                     Contract
 --                       { parties = [ "Company" , "Alice" , "Bob" ]
 --                       , dated = 1970 (-01) (-02)
 --                       , title = "shareholdersAgreement"
 --                       , singleton = True
 --                       }
 --

applyRules :: [Rule] -> Tree DiffBox -> Deps
applyRules rulebase difftree =
  -- crunch through every diff in the tree
  -- testing every rule in the rulebase
  
  concat $ diff2paper <$> difftree
  where diff2paper :: DiffBox -> Deps
        diff2paper diff = do
          rule <- rulebase
          let (RuleDiff temporal matchDiff paper) = rule
          guard (matchDiff diff)
          return paper





{- changes to the Company -}

-- change of address requires directors resolution
-- and requires notification to all parties to all agreements

{- changes to Holders -}

{- changes to a particular Holder -}

{- changes to Securities -}

{- changes to a particular Security -}

{- changes to the Holder/Security relation -}

{- changes to a particular Security -}

{- changes to Agreements -}

{- changes to a particular Agreement -}

-- adding a new shareholder to a shareholdersAgreement
-- requires a deed of ratification and accession.
-- note that a shareholdersAgreement is a singleton
-- and therefore shows up as an Update rather than a Delete / Create




