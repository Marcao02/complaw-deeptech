{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}

module BusinessLogic where

-- for the company data model
import Company
import CDiff
import Control.Monad.Reader
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

type MkPaperwork a = Reader a Paperwork

data Paperwork = Paperwork { ptitle :: PaperworkName
                           , dr :: [DirectorsResolution]
                           , mr :: [MembersResolution]
                           , ag :: [Agreement]
                           , nt :: [Notice]
                           }
               deriving (Show)

type PaperworkName = String

data Temporal = XY需要
              | ForceWithin DurationSpec
              | Simul
              deriving (Show)

data DurationSpec = DurationYMD Int Int Int
                  deriving (Show)

-- temporal XY需要      X Y means: before we can do a thing that matches X, we must first do Y.  basically, X requires Y.
-- temporal ForceWithin X Y means:  after we     do a thing that matches X, we must next  do Y by DurationSpec, as a relative deadline
-- temporal Simul       X Y means:   when we     do a thing that matches X, we must simultaneously do Y

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
                         , matchdiff :: Diff -> Bool   -- does this rule fire against the given input diff?
                         , temporal :: Temporal
                         , requires :: PaperworkName
                         }

diffRules :: [DiffRule]
diffRules = [ DiffRule "SHA changed"       shaChanges  XY需要 "ratify DORA"
            , DiffRule "new shareholder"   newHoldings XY需要 "actual investment agreements"
            ]
  where
    shaChanges (Diff Update old new comment) =
      -- https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Typeable.html
      fromMaybe False $ do mc <- (cast new :: Maybe (Maybe Contract))
                           c  <- mc
                           return $ title (c :: Contract) == "shareholdersAgreement"
    shaChanges _ = False

    -- rule fires against a list of Holdings
    -- Holdings / Holding / HeldSecurities / HeldSecurity
    newHoldings (Diff Update old new comment) =
      fromMaybe False $ do mh <- (cast new :: Maybe (Maybe [Holding]))
                           holdings <- mh
                           return (length holdings > 0)
    newHoldings _ = False

diffTree2Paperworks :: Map.Map PaperworkName (MkPaperwork (Tree Diff))
diffTree2Paperworks = Map.fromList [
  let ptitle = "ratify DORA" in (
    ptitle, do
        dt <- ask
        return (Paperwork ptitle
          ([DR "company to circulate deed of ratification and accession"
                "resolved, that the company should circulate a deed of ratification and accession to the shareholders agreement"
           ,DR "company to sign aforesaid deed"
                "resolved, that the company should ratify the aforesaid deed"
           ])
          ([] :: [MembersResolution])
          ([Ag "DORA"
               "The signatories hereby ratify and accede to the Shareholders Agreement."
               (newParties $ dt)
               Deed ])
          ([] :: [Notice])))
  ,let ptitle = "actual investment agreements" in (
      ptitle, do
          dt <- ask
          return (Paperwork ptitle
            ([] :: [DirectorsResolution])
            ([] :: [MembersResolution])
            ([Ag "Investment Agreement"
              "The actual investment agreements between the Company and the investors in the new round."
              ("Company" : (investorsInRound $ dt))
              AgContract ])
            ([] :: [Notice])
                 )
      )
   ]

data RuleOrigin = RO { originName    :: String
                     , originURL     :: String
                     , originsection :: String
                     }

data PaperRule = PaperRule { rulename :: String
                           , matchpaperwork :: PaperworkName
                           , temporal :: Temporal
                           , requires :: PaperworkName
                           , ruleOrigins :: [RuleOrigin] }

-- 185.  Where by this Act special notice is required of a resolution, the resolution shall not be effective unless notice of the intention to move it has been given to the company not less than 28 days before the meeting at which it is moved, and the company shall give its members notice of any such resolution at the same time and in the same manner as it gives notice of the meeting or, if that is not practicable, shall give them notice thereof, in any manner allowed by the constitution, not less than 14 days before the meeting, but if after notice of the intention to move such a resolution has been given to the company, a meeting is called for a date 28 days or less after the notice has been given, the notice, although not given to the company within the time required by this section, shall be deemed to be properly given.


paperworks2Paperworks :: Map.Map PaperworkName (MkPaperwork (Tree Diff,Paperwork))
paperworks2Paperworks = Map.fromList [
  let ptitle = "pro rata rights" in (
    ptitle, do
        (dt,p1) <- ask
        return $ Paperwork ptitle
          ([] :: [DirectorsResolution])
          ([] :: [MembersResolution])
          ([] :: [Agreement])
          ([Notice "Pro Rata Rights Notice"
            "The company proposes to issue new securities. As a member of the company, you have the right to participate. To maintain your proportional allocation, you would need to invest X. But you can indicate your interest in participating up to the full amount on offer -- if any other members do not choose to re-up, we will attempt to satisfy your excess interest"
            (oldEquityHolders dt)
           ] :: [Notice])
          )

  ,let ptitle = "shareholder approval" in (
      ptitle, do
        (dt,p1) <- ask
        return $ Paperwork ptitle
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

  ,let ptitle = "director resolutions" in (
      ptitle, do
          (dt,p1) <- ask
          return $ Paperwork ptitle
            [DR "company to raise funds"
             "resolved, that the company should raise funds by issuing XXXX amount of new securities"
            ,DR "company to seek members' approval"
             "resolved, that the approval of the members be sought through an EGM or equivalent resolutions by written means"
            ]
            ([] :: [MembersResolution])
            ([] :: [Agreement])
            ([] :: [Notice])
      )
  ]

paperRules = [ PaperRule "prorata rights"          "actual investment agreements" XY需要 "pro rata rights"      [(RO "company constitution" "https://sso.agc.gov.sg/SL/CoA1967-S833-2015" "45")]
-- 45.—(1)  Subject to any direction to the contrary that may be given by the company in general meeting, all new shares must, before issue, be offered to all persons who, as at the date of the offer, are entitled to receive notices from the company of general meetings, in proportion, or as nearly as the circumstances admit, to the amount of the existing shares to which they are entitled.

             , PaperRule "shareholder approval"    "pro rata rights"              XY需要 "shareholder approval" [(RO "50 companies act" "https://sso.agc.gov.sg/Act/CoA1967?ProvIds=pr161-#pr161-" "161")]
-- Notwithstanding anything in a company’s constitution, the directors shall not, without the prior approval of the company in general meeting, exercise any power of the company to issue shares.

             , PaperRule "director proposal"       "shareholder approval"         XY需要 "director resolutions" [(RO "company constitution" "https://sso.agc.gov.sg/SL/CoA1967-S833-2015" "7")
-- 7.—(1)  Without prejudice to any special rights previously conferred on the holders of any existing shares or class of shares but subject to the Act, shares in the company may be issued by the directors.
                                                                                                                ,(RO "50 companies act" "https://sso.agc.gov.sg/Act/CoA1967?ProvIds=pr184C-#pr184C-" "184C") ]
-- 184C.—(1)  The directors of a private company or an unlisted public company who wish to seek agreement to a resolution of the company and for it to be passed by written means shall send to each member, having the right to vote on that resolution at a general meeting, a copy of the text of the resolution.
               
             ]



oldEquityHolders :: Tree Diff -> [PartyName]
oldEquityHolders csdt = trace ("oldEquityHolders: trying this thing " ++ (show $ rootLabel csdt)) $ case rootLabel csdt of
  boop@(Diff crud old new comment) -> let oldCS = do cs <- (cast old :: Maybe CompanyState)
                                                     trace ("cast succeeded; " ++ show cs) $ return cs
                                      in trace ("oldEquityHolders: got myself a companystate") $ case oldCS of Just cs -> holder <$> holdings cs
                                                                                                               _ -> ["blarp"]
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


-- when constructing a Paperwork from its template,
-- we give the Paperwork generator the following context:
   -- the top level root Diff, which is always a CompanyState
   -- the paperwork dependency graph as at the time of the construction of that Paperwork
   -- the parent node within that graph that most proximally motivated the generation

applyDrules :: [DiffRule] -> Tree Diff -> Deps
applyDrules rulebase difftree =
  -- crunch through every diff in the tree
  -- testing every diff rule in the rulebase
  let rootdeps = do
        rule <- rulebase
        guard $ matchdiff rule (rootLabel difftree)
        return $ runReader (diffTree2Paperworks Map.! (requires (rule :: DiffRule))) difftree
      children = concat $ applyDrules rulebase <$> subForest difftree
   in concat [rootdeps, children]

applyPrules :: Tree Diff -> [PaperRule] -> [Paperwork] -> Maybe ([Paperwork], [Paperwork])
applyPrules cs_treediff rulebase paperworks =
  let deps = concatMap (applyPrule cs_treediff rulebase) paperworks
  in if (length deps == 0)
     then Nothing
     else Just (deps, deps)

applyPrule :: Tree Diff -> [PaperRule] -> Paperwork -> [Paperwork]
applyPrule companystate_treediff rulebase pdep = do
  rule <- rulebase
  trace ("paperwork dependency " ++ ptitle pdep ++ "; testing rule " ++ rulename (rule :: PaperRule)) $
        guard $ matchpaperwork rule == ptitle (pdep :: Paperwork)
  trace ("rule fired; requires " ++ requires (rule :: PaperRule)) $ return $ runReader (paperworks2Paperworks Map.! (requires (rule :: PaperRule))) (companystate_treediff, pdep)
  
      
{- THE FOLLOWING ARE TODO -}

{- changes to the Company -}

{-    change of address -}
--    change of address requires directors resolution
--    and requires notification to all parties to all agreements
--    and requires updating the government as well so the updating of the government itself needs to be authorized.


{-    change of director -}
{-    change of secretary -}
{-    change of auditor -}



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




