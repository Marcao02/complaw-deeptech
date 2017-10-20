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
import Data.Maybe
import Debug.Trace (trace)

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
                    , parties :: [PartyName]
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

data Rule = RuleDiff { rulename :: String
                     , temporal :: Temporal
                     , matchdiff :: MatchDiff
                     , d2p :: Tree Diff -> Paperwork
                     }
          | RulePaper { rulename :: String
                      , temporal :: Temporal
                      , matchpaperwork :: MatchPaperwork
                      , p2p :: Paperwork -> Paperwork }

-- temporal Pre X Y means: before we can do a thing that matches X, we must first do Y
-- temporal Post  X Y means:  after we     do a thing that matches X, we must next  do Y by DurationSpec, as a relative deadline
-- temporal Simul X Y means:   when we     do a thing that matches X, we must simultaneously do Y

type MatchDiff      = Diff -> Bool
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
ruleBase = [ RuleDiff "SHA changed" Pre shaChanged
             (\difftree -> Paperwork
              [ DR "company to circulate deed of ratification and accession" "resolved, that the company should circulate a deed of ratification and accession to the shareholders agreement" 0
              , DR "company to sign aforesaid deed" "resolved, that the company should ratify the aforesaid deed" 1
              ]
              ([] :: [MembersResolution])
              [Ag
               "DORA"
               "The signatories hereby ratify and accede to the Shareholders Agreement."
               (newParties $ difftree)
               2
               Deed
              ] 0) ]
  where
    shaChanged :: MatchDiff
    shaChanged (Diff Update old new comment) =
      -- https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Typeable.html
      fromMaybe False $ do mc <- (cast new :: Maybe (Maybe Contract))
                           c  <- mc
                           return $ title (c :: Contract) == "shareholdersAgreement"
    shaChanged _ = False

newParties :: Tree Diff -> [PartyName]
newParties contract_tdb = catMaybes $ catMaybes $ do
  -- extract the child diffbox matching "xpath"
  -- CompanyState / Contracts / Contract.title="shareholdersAgreement" / PartyNames / Create
  partyNames <- subForest contract_tdb; guard $ comment (rootLabel partyNames) == "changed PartyNames"
  partyName  <- subForest partyNames;   guard $ comment (rootLabel partyName)  == "from Nothing"
  return $ ((\diff -> case diff of
                (Diff Create old new comment) -> (cast new :: Maybe (Maybe String))
                _ -> Nothing
            ) (rootLabel partyName))

applyRules :: [Rule] -> Tree Diff -> Deps
applyRules rulebase difftree =
  -- crunch through every diff in the tree
  -- testing every rule in the rulebase
  let rootdeps = do
        rule <- rulebase
        guard (matchdiff rule (rootLabel difftree))
        return (d2p rule difftree)
      children = concat $ applyRules rulebase <$> subForest difftree
  in concat [rootdeps, children]
    
     

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




