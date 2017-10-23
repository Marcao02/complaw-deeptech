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
import qualified Data.Map as Map
import Data.List (nub, intercalate)
import Debug.Trace (trace)

-- graph
import Data.Graph
import Data.Tree

-- what paperwork is required to effect a change?

data DirectorsResolution = DR { title :: String
                              , body  :: String
                              } deriving (Show)
                           
data MembersResolution = MR { title :: String
                            , body  :: String
                            , level :: MRlevel
                            } deriving (Show)

data MRlevel = Ordinary | Special
             deriving (Show)

data Agreement = Ag { title :: String
                    , body  :: String
                    , parties :: [PartyName]
                    , level :: AgLevel
                    } deriving (Show)

data AgLevel = LOI | AgContract | Deed
             deriving (Show)

data Notice = Notice { title :: String
                     , body  :: String
                     , parties :: [PartyName]
                     } deriving (Show)

data TreeDiff2Paperwork = TreeDiff2Paperwork { ptitle :: Diff Tree -> String
                                             , dt2dr :: [Diff Tree -> DirectorsResolution]
                                             , dt2mr :: [Diff Tree -> MembersResolution]
                                             , dt2ag :: [Diff Tree -> Agreement]
                                             , dt2nt :: [Diff Tree -> Notice]
                                             }

data Paperwork2Paperwork = Paperwork2Paperwork { ptitle :: Paperwork -> String
                                               , dt2dr :: [Paperwork -> DirectorsResolution]
                                               , dt2mr :: [Paperwork -> MembersResolution]
                                               , dt2ag :: [Paperwork -> Agreement]
                                               , dt2nt :: [Paperwork -> Notice]
                                               }

-- later, we have functions that concretize the above templates (TreeDiff2Paperwork and Paperwork2Paperwork)
-- into concrete Paperwork objects

data Paperwork = Paperwork { ptitle :: String
                           , dr :: [DirectorsResolution]
                           , mr :: [MembersResolution]
                           , ag :: [Agreement]
                           , nt :: [Notice]
                           }
               deriving (Show)

-- it feels like this would be a good opportunity to apply the Reader Monad; am I on the right track?


data Temporal = FirstNeed
              | ForceWithin DurationSpec
              | Simul
              deriving (Show)

data DurationSpec = DurationYMD Int Int Int
                  deriving (Show)

-- temporal Pre X Y means: before we can do a thing that matches X, we must first do Y
-- temporal Post  X Y means:  after we     do a thing that matches X, we must next  do Y by DurationSpec, as a relative deadline
-- temporal Simul X Y means:   when we     do a thing that matches X, we must simultaneously do Y

type MatchDiff      = Diff -> Bool

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

data DiffRule = DiffRule { rulename :: String
                         , matchdiff :: MatchDiff
                         , temporal :: Temporal
                         , d2p :: Tree Diff -> PaperworkName
                         }
diffRules :: [DiffRule]
diffRules = [ DiffRule "SHA changed"             shaChanges                 FirstNeed ratifyDORA
            , DiffRule  "new shareholder"        newHoldings                FirstNeed investmentAgreements
            ]
  where
    shaChanges (Diff Update old new comment) =
      -- https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Typeable.html
      fromMaybe False $ do mc <- (cast new :: Maybe (Maybe Contract))
                           c  <- mc
                           return $ title (c :: Contract) == "shareholdersAgreement"
    shaChanges _ = False

    ratifyDORA contractDiff = "ratify DORA"

    -- Holdings / Holding / HeldSecurities / HeldSecurity
    newHoldings (Diff Update old new comment) =
      fromMaybe False $ do mh <- (cast new :: Maybe (Maybe [Holding]))
                           holdings <- mh
                           return (length holdings > 1)
    newHoldings _ = False

    investmentAgreements holdingDiff = actualInvestmentAgreements

type PaperworkName = String

data PaperRule = PaperRule { rulename :: String
                           , matchpaperwork :: PaperworkName
                           , temporal :: Temporal
                           , requires :: PaperworkName }

diffTree2Paperworks = Map.fromList [
  let ptitle = "ratify DORA" in (
    ptitle, TreeDiff2Paperwork (\dt -> ptitle)
      ([ \dt -> DR "company to circulate deed of ratification and accession"
            "resolved, that the company should circulate a deed of ratification and accession to the shareholders agreement"
       , \dt -> DR "company to sign aforesaid deed"
            "resolved, that the company should ratify the aforesaid deed"
       ])
      (\dt -> [] :: [Tree Diff -> MembersResolution])
      ([\dt -> Ag "DORA"
           "The signatories hereby ratify and accede to the Shareholders Agreement."
           (newParties $ dt)
           Deed ])
      (\dt -> [] :: [Tree Diff -> Notice]))
  ,
  let ptitle = "actual investment agreements" in (
    ptitle, TreeDiff2Paperwork (\dt -> ptitle)
      ([] :: [Tree Diff -> DirectorsResolution])
      ([] :: [Tree Diff -> MembersResolution])
      ([\dt -> Ag "Investment Agreement"
        "The actual investment agreements between the Company and the investors in the new round."
        ("Company" : (investorsInRound $ dt))
        AgContract ])
      ([] :: [Tree Diff -> Notice])
    )
  ]

paperworks2Paperworks = Map.fromList [                                                  
  let ptitle = "pro rata rights" in (
    ptitle, Paperwork ptitle
   ([] :: [DirectorsResolution])
   ([] :: [MembersResolution])
   ([] :: [Agreement])
   ([Notice "Pro Rata Rights Notice"
            "The company proposes to issue new securities. As a member of the company, you have the right to participate. To maintain your proportional allocation, you would need to invest X. But you can indicate your interest in participating up to the full amount on offer -- if any other members do not choose to re-up, we will attempt to satisfy your excess interest"
           (oldEquityHolders csdt)
    ] :: [Notice])
  )
  ,let ptitle = "shareholder approval" in (
      ptitle, Paperwork ptitle
      ([] :: [DirectorsResolution])
      ([MR "issue of new securities approved"
        "Resolved, that the Directors be empowered to raise funds via an issue of new securities"
        Special
       ,MR "conversion of securities to equity approved"
        "Resolved, that the Company issue equity securities in the future as needed to satisfy the terms of conversion described by the above securities"
        Special
       ] :: [MembersResolution])
      ([] :: [Agreement])
      ([] :: [Notice])
      )
  , let ptitle = "director resolutions" in (
      ptitle, Paperwork ptitle
      ([DR "company to raise funds"
           "resolved, that the company should raise funds by issuing XXXX amount of new securities"
       ,DR "company to seek members' approval"
           "resolved, that the approval of the members be sought through an EGM or equivalent resolutions by written means"
       ])
      ([] :: [MembersResolution])
      ([] :: [Agreement])
      ([] :: [Notice])
      )
  ]

paperRules = [ PaperRule "prorata rights (excess)" "actual investment agreements" FirstNeed "pro rata rights"
             , PaperRule "shareholder approval"    "pro rata rights"              FirstNeed "shareholder approval"
             , PaperRule "director proposal"       "shareholder approval"         FirstNeed "director resolutions"
             ]
  where
-- make Dep a tuple containing the motivating difftree
    equityInvestmentAgreements (Paperwork pt drs mrs ags notices) =
      any (\ag -> case ag of (Ag "Investment Agreement" _ _ _) -> True; _ -> False) ags

    prorataNormal csdt postpaper =

    prorataOffer (Paperwork "pro rata rights" drs mrs ags notices) = True
    prorataOffer _ = False

    shareholderApproval csdt postpaper =

    shareholderApprovalResos (Paperwork "shareholder approval" _ _ _ _) = True
    shareholderApprovalResos _ = False

    directorApproval csdt postpaper =


oldEquityHolders :: Tree Diff -> [PartyName]
oldEquityHolders csdt = case rootLabel csdt of
  (Diff crud old new comment) -> let oldCS = do cs <- (cast old :: Maybe CompanyState)
                                                return cs
                                 in case oldCS of Just cs -> holder <$> holdings cs
                                                  _ -> []
  _ -> []

{- sequence for new investor
1. (directors resolution) directors agree to issue new shares
2. (directors resolution) directors request permission from members to issue new shares
3. (members resolutions) members either hold an extraordinary general meeting or pass resolutions by written means approving the directors' plan
4. (company notice to debtholders) if the new share issue triggers any convertible debt, company informs debtholders and revises the shareholder roster accordingly
4. (company notice to shareholders) directors, writing on behalf of company, offer existing members pro-rata rights in the new share issue
5. (company notice to shareholders) directors offer existing members excess rights in the new share issue
6. (company notice to non-members) directors offer non-members participation in the new share issue
7. (company generates and executes agreement) new investors sign DORA
8. (company generates and executes agreement) old investors sign DORA
9. (company generates and executes agreement) directors sign investment agreement with participating investors
10. company files updated shareholding roster with acra
-}

investorsInRound :: Tree Diff -> [PartyName]
investorsInRound holdings = nub $ catMaybes $ do
  heldsecurity <- subForest holdings
  let mmhss = (\diff -> case diff of
                  (Diff Create Nothing new comment) -> cast new :: Maybe (Maybe [HeldSecurity])
                  _ -> Nothing
              ) (rootLabel heldsecurity)
      mhss = fromMaybe Nothing mmhss
      hss  = fromMaybe [] mhss
  hs <- hss
  return $ do
    desc <- description hs
    let d = head $ words desc
    return d
                    
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

-- hm, maybe i want Pipes

applyDrules :: [DiffRule] -> Tree Diff -> Deps
applyDrules rulebase difftree =
  -- crunch through every diff in the tree
  -- testing every diff rule in the rulebase
  let rootdeps = do
        rule <- rulebase
        guard $ matchdiff rule (rootLabel difftree)
        return $ d2p rule difftree
      children = concat $ applyDrules rulebase <$> subForest difftree
   in concat [rootdeps, children]

-- TODO: this applies once. we need to keep applying until the dependency list stabilizes, i.e. stops growing
applyPrules :: Tree Diff -> [PaperRule] -> [Paperwork] -> Maybe ([Paperwork], [Paperwork])
applyPrules cs_treediff rulebase paperworks =
  let deps = concatMap (applyPrule cs_treediff rulebase) paperworks
  in if (length deps == 0)
     then Nothing
     else Just (deps, deps)

applyPrule :: Tree Diff -> [PaperRule] -> Paperwork -> [Paperwork]
applyPrule companystate_treediff rulebase pdep = do
  rule <- rulebase
  trace ("testing rule " ++ rulename (rule :: PaperRule) ++ " against paperwork dependency " ++ ptitle pdep) $
        guard $ matchpaperwork rule == rulename pdep
  return $ p2p rule companystate_treediff pdep
  
      

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




