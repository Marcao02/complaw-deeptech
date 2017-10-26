{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

-- emacs haskell-mode with intero
-- C-c ! n for next flycheck error
-- C-c ! p for prev flycheck error

module CDiff where

import Debug.Trace
import Company
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.Tree
import Data.Maybe
import Data.Semigroup
import Data.List (foldl1', foldl1)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Merge
import qualified Data.Set as Set
import Data.Aeson (ToJSON)
import qualified Data.Text as T (unpack)

type Holder = Party

data Relation = CH Company Holder
              | CS Company Security
              | HS Holder  Security
  deriving (Show, Eq)

data CRUD = Create | Replace | Update | Delete
            deriving (Show, Typeable)

data Diff where
  Diff :: (RDiff a, Show a, Typeable a) => { crud :: CRUD
                                           , old  :: Maybe a
                                           , new  :: Maybe a
                                           , comment :: String } -> Diff
  Noop :: { comment :: String } -> Diff

instance Show Diff where
  show (Diff crud old new comment) = unwords ["Diff", (show crud), show old, show new, show comment]
  show (Noop comment) = unwords ["Noop", show comment]

pruneNoops :: Tree Diff -> Maybe (Tree Diff)
pruneNoops (Node (Noop _)     forest)   = Nothing
pruneNoops (Node d forest) = Just $ Node d (catMaybes $ map pruneNoops forest)

-- Recursively Diffable types
class RDiff x where
  rdiff :: x -> x -> Tree Diff

mkDiff :: (RDiff a, Show a, Typeable a) => String -> a -> a -> [Tree Diff] -> Tree Diff
mkDiff elName x y forest =
  let pruned = catMaybes $ pruneNoops <$> forest
  in mytrace ("mkDiff " ++ elName)
     (if length pruned > 0
      then Node (Diff Update (Just x) (Just y) ("changed " ++ elName))
      else Node (Noop ("no change to " ++ elName))
     ) pruned

instance RDiff () where
  rdiff x y = Node (Noop "") []

-- argh, this doesn't work.
-- instance RDiff DiffBox where
--   rdiff db1@(DiffBox diff1) db2@(DiffBox diff2) = rdiff diff1 diff2

-- maybe we need https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Data.html to "Scrap your boilerplate"

instance RDiff CompanyState where
  rdiff
    s1@(CompanyState holders1 se1 coy1 holdings1 contracts1)
    s2@(CompanyState holders2 se2 coy2 holdings2 contracts2) =
    mkDiff "CompanyState" s1 s2 [rdiff holders1 holders2
                                ,rdiff se1 se2
                                ,rdiff coy1 coy2
                                ,rdiff holdings1 holdings2
                                ,rdiff contracts1 contracts2
                                ]
instance RDiff [ConExp] where
  rdiff s1s s2s =
    let mergedConstitution = pairBy (title :: ConExp -> String) (:[]) s1s s2s
    in mkDiff "Constitution" s1s s2s [ rdiff (fmap head cBefore) (fmap head cAfter)
                                     | (cName,(cBefore,cAfter)) <- Map.assocs mergedConstitution ]

instance RDiff ConExp where
  rdiff
    s1@(ConExp title1 body1)
    s2@(ConExp title2 body2) =
    mkDiff "ConExp" s1 s2 [rdiff body1 body2]

                                       
instance RDiff Company where
  rdiff
    s1@(Company n1 j1 address1 idtype1 idnum1 constitution1 directors1 secretary1)
    s2@(Company n2 j2 address2 idtype2 idnum2 constitution2 directors2 secretary2) =
    mkDiff "Company" s1 s2 [rdiff n1 n2
                           ,rdiff j1 j2
                           ,rdiff address1 address2
                           ,rdiff idtype1 idtype2
                           ,rdiff idnum1 idnum2
                           ,rdiff constitution1 constitution2
                           ,rdiff directors1 directors2
                           ,rdiff secretary1 secretary2
                           ]
  
instance RDiff [Party] where
  rdiff s1s s2s =
    let mergedHolders = pairBy fullname (:[]) s1s s2s
    in mkDiff "Holders" s1s s2s [ rdiff (fmap head hBefore) (fmap head hAfter)
                                | (hName,(hBefore,hAfter)) <- Map.assocs mergedHolders ]

instance RDiff Party where
  rdiff
    s1@(Party fullname1 idtype1 idnum1 nature1 gender1)
    s2@(Party fullname2 idtype2 idnum2 nature2 gender2) =
    mkDiff "Holder" s1 s2 [rdiff fullname1 fullname2
                          ,rdiff idtype1 idtype2
                          ,rdiff idnum1 idnum2
                          ,rdiff nature1 nature2
                          ,rdiff gender1 gender2]


instance RDiff [Contract] where
  rdiff s1s s2s =
    let mergedContracts = pairBy (\c -> if singleton c
                                        then (title (c :: Contract))
                                        else (title (c :: Contract)) ++ (show $ dated c)) (:[]) s1s s2s
    in mkDiff "Contracts" s1s s2s [ rdiff (fmap head cBefore) (fmap head cAfter)
                                  | (cName,(cBefore,cAfter)) <- Map.assocs mergedContracts ]


instance RDiff Contract where
  rdiff
    s1@(Contract parties1 dated1 title1 singleton1)
    s2@(Contract parties2 dated2 title2 singleton2) =
    mkDiff "Contract" s1 s2 [rdiff parties1 parties2
                            ,rdiff (show dated1) (show dated2)
                            ,rdiff title1 title2
                            ,rdiff singleton1 singleton2]
  
instance RDiff [PartyName] where
  rdiff s1s s2s =
    let merged = pairBy id (:[]) s1s s2s
    in mkDiff "PartyNames" s1s s2s [ rdiff (fmap head hBefore) (fmap head hAfter)
                                   | (hName,(hBefore,hAfter)) <- Map.assocs merged ]

instance RDiff [Security] where
  rdiff s1s s2s =
    let mergedSecurities = pairBy (name :: Security -> String) (:[]) s1s s2s
    in mkDiff "Securities" s1s s2s [ rdiff (fmap head sBefore) (fmap head sAfter)
                                   | (sName,(sBefore,sAfter)) <- Map.assocs mergedSecurities ]

instance RDiff Security where
  rdiff s1@(Security name1 measure1)
        s2@(Security name2 measure2) =
    mkDiff "Security" s1 s2 [rdiff name1 name2]
  

instance RDiff [Holding] where
  rdiff s1s s2s =
    mkDiff "Holdings" s1s s2s [ rdiff h1 h2
                              | (hname,(h1,h2)) <- Map.assocs $ pairBy holder holds s1s s2s ]

instance RDiff Holding where
  rdiff s1@(Holding holder1 holds1)
        s2@(Holding holder2 holds2) =
    let holdingsBySN = (pairBy securityName (:[]) holds1 holds2)
    -- :: Map holderName ( Map securityName ( [HeldSecurity/before] , [HeldSecurity/after] ) )

    in mkDiff "Holding" s1 s2 [ rdiff hsBefore hsAfter
                              | (hsName,(hsBefore,hsAfter)) <- Map.assocs holdingsBySN
                              ]

ignoreDescription :: Maybe HeldSecurity -> Maybe HeldSecurity
ignoreDescription (Just r) = -- mytrace ("ignoreDescription: flattening description="++(show $ description r))
                             Just (r { description = Nothing })
ignoreDescription x = x

myconcat :: Maybe [HeldSecurity] -> Maybe HeldSecurity
myconcat Nothing = Nothing
myconcat (Just []) = Nothing
myconcat (Just hslist) = Just (foldl1' (<>) hslist)

-- HeldSecurities have either Just units or Just money but not both; this semigroup sums either way
instance Semigroup HeldSecurity where
   hs1 <> hs2 = hs2 { units = maybeSum (units hs1) (units hs2),
                      money = maybeSum (money hs1) (money hs2) }

maybeSum :: (Num a) => Maybe a -> Maybe a -> Maybe a
maybeSum Nothing _ = Nothing
maybeSum _ Nothing = Nothing
maybeSum (Just a) (Just b) = Just (a+b)

instance RDiff [HeldSecurity] where
  rdiff hs1s hs2s =
    let hsBefore = ignoreDescription $ myconcat $ Just hs1s
        hsAfter  = ignoreDescription $ myconcat $ Just hs2s
    in
      mkDiff "HeldSecurities" hs1s hs2s [rdiff hsBefore hsAfter ]
  
instance RDiff HeldSecurity where
  rdiff s1@(HeldSecurity securityName1 units1 money1 description1)
        s2@(HeldSecurity securityName2 units2 money2 description2) =
    mkDiff "HeldSecurity" s1 s2 [rdiff units1 units2
                                ,rdiff money1 money2
                                ,rdiff description1 description2 ]


instance RDiff EntityNature where
  rdiff s1@Human     s2@Corporate   = Node (Diff Replace (Just s1) (Just s2) "incorporated") []
  rdiff s1@Corporate s2@Human       = Node (Diff Replace (Just s1) (Just s2) "disincorporated") []
  rdiff s1@Human     s2@AI          = Node (Diff Replace (Just s1) (Just s2) "uploaded") []
  rdiff s1@Corporate s2@AI          = Node (Diff Replace (Just s1) (Just s2) "blockchained") []
  rdiff s1@AI        s2@Human       = Node (Diff Replace (Just s1) (Just s2) "embodied") []
  rdiff s1@AI        s2@Corporate   = Node (Diff Replace (Just s1) (Just s2) "downgraded") []
  rdiff s1           s2 | s1 == s2  = Node (Noop "no change to EntityNature") []
                        | otherwise = Node (Diff Replace (Just s1) (Just s2) "changed somehow") []
    
instance RDiff Gender where
  rdiff s1           s2 | s1 == s2     = Node (Noop "no change to Gender") []
  rdiff s1@Male    s2@Female           = Node (Diff Replace (Just s1) (Just s2) "m2f") []
  rdiff s1@Female  s2@Male             = Node (Diff Replace (Just s1) (Just s2) "f2m") []
  rdiff s1         s2@Neutral          = Node (Diff Replace (Just s1) (Just s2) "unsexed") []
  rdiff s1         s2@Male             = Node (Diff Replace (Just s1) (Just s2) "androgened") []
  rdiff s1         s2@Female           = Node (Diff Replace (Just s1) (Just s2) "estrogened") []
  rdiff s1         s2@(OtherGender og) = Node (Diff Replace (Just s1) (Just s2) ("became " ++ og)) []

-- if we have an Old thing and a New thing
-- where there is a key Old.key and New.key
-- and an array of values Old.array and New.array
-- it is possible to reorganize the things into a map of key:(Maybe oldArray, Maybe newArray)

-- first we get out the key, oldArray, and newArray
           
pairBy :: (Ord b) => (a -> b) -> (a -> [c]) -> [a] -> [a] -> Map.Map b (Maybe [c], Maybe [c])
pairBy getName getKids old new =
  merge2ways (pairup old) (pairup new)
  where pairup record = Map.fromListWith (++) $ fmap (\r -> (getName r, getKids r)) record

-- if we have two maps of key:oldArray and key:newArray
-- we merge the two maps into a map of key:(Maybe oldArray, Maybe newArray).
-- we handle situations where the old does not exist or the new does not exist by setting Nothing
merge2ways oldMap newMap =
        Merge.merge
        (Merge.mapMissing (\k x -> (Just x, Nothing)))
        (Merge.mapMissing (\k y -> (Nothing, Just y)))
        (Merge.zipWithMatched (\k x y -> (Just x, Just y)))
        oldMap
        newMap
      




-- we should be thinking about certain situations in terms of relations rather than
-- blindly recursing through the CompanyState tree.

-- there are of course three relations of interest
-- CH company/holder
-- CS company/security
-- HS holder /security

-- CH create <=> HS create



instance (Show a, RDiff a, Typeable a, RDiff a) => RDiff (Maybe a) where
  rdiff Nothing Nothing   = Node (Noop "given identical Nothing inputs") []
  rdiff Nothing (Just y)  = Node (Diff Create Nothing (Just y) "from Nothing") []
  rdiff (Just x) Nothing  = Node (Diff Delete (Just x) Nothing   "to Nothing") []
  rdiff (Just x) (Just y) = rdiff x y

-- syntactic sugar
ndr :: (Show term, Eq term, Typeable term, RDiff term) => term -> term -> String -> Tree Diff
ndr s1 s2 str | s1 == s2  = Node (Noop "no change") []
              | otherwise = Node (Diff Replace (Just s1) (Just s2) str)  []

instance RDiff String where
  rdiff s1 s2 = ndr s1 s2 (show "change string")

instance RDiff Float where
  rdiff s1 s2 = ndr s1 s2 (show ("change float from " ++ show s1 ++ " to " ++ show s2))
  
instance RDiff Int where
  rdiff s1 s2 = ndr s1 s2 (show "change int")
  
instance RDiff Bool where
  rdiff s1 s2 = ndr s1 s2 (show "change bool")
  
  
{-
Prelude Data.Tree> :browse Data.Tree
type Forest a = [Tree a]
data Tree a = Node {rootLabel :: a, subForest :: Forest a}
drawForest :: Forest String -> String
drawTree :: Tree String -> String
flatten :: Tree a -> [a]
levels :: Tree a -> [[a]]
unfoldForest :: (b -> (a, [b])) -> [b] -> Forest a
unfoldForestM ::
  Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
unfoldForestM_BF ::
  Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTreeM :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM_BF :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
-}
  
-- diffs2actions :: [Diff] -> [Action]

debugging = False
mytrace x y = if debugging then trace x y else y


