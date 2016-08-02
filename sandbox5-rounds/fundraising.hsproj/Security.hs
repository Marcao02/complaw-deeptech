module Security where

import Data.List.Split
import Data.List
import Money
import qualified NaturalLanguage    as NL -- most of the output related code lives here
import qualified Data.Maybe as Maybe
 
import qualified Data.Time.Calendar as Calendar

-- ====================================================================== SECURITY

-- later we will have security combinators, to represent an instrument whose attributes may change based on circumstance
-- for GAAP / IFRS purposes we want to be able to characterize both individual simple Securities and combinations of securities.
-- a security combinator may be threaded together with a modal combinator.

data Right = Voting Int | Board | OtherRight String
             deriving (Show)

type NumMonths = Int

data Security = Security { securityName    :: String
                         , preMoney        :: Maybe Money
                         , discount        :: Maybe Int -- numerator /100
                         , term            :: Maybe NumMonths -- TODO: change this to a dateinterval | fixedDate
                         , redeemable      :: Bool
                         , rights          :: [Right]
                         , natureHardcoded :: Maybe SecurityNature
                         }
              deriving (Show)


-- how do we express something like this?
-- security and commercial terms: convertible note
-- converting at first equity fundraise of more than $1M,
-- where pre-money is over $3.5M,
-- at a fixed cap of $3.5M,
-- with pro-rata rights in the qualifying round,
-- to a form of the qualifying-round security without voting rights
 


--
-- TODO: add national and linguistic context to a security
-- because a security is scoped nationally
-- we probably need to be clever about AbstractSecurity vs a NationalSecurity vs a ConcreteSecurity which has a specific contract with parties
-- 

instance NL.Lang Security where
 to lang security = unlines [ "Security: " ++ NL.nameOf security ++ " (" ++ NL.to lang (securityNature security) ++ ")"
                            , sayPreMoney lang $ preMoney security
                            ]
   
sayPreMoney :: NL.Ctx -> Maybe Money -> String
sayPreMoney lang (Just m) = "pre-money cap of " ++ NL.to lang m
sayPreMoney lang Nothing  = "(no pre-money cap)"


instance NL.Lang Calendar.Day where
  to lang@(NL.Ctx { NL.lang=NL.English }) = show
  to _ = show


instance NL.Named Security where nameOf = securityName

-- ---------------------------------------------------------------------- SecurityNature

-- if we don't specify if it's debt or equity, the nature of the security is interpretive
data SecurityNature = Ordinary | Preferred | Debt | Equity | Unknown
                    deriving (Show)
securityNature (Security { natureHardcoded = Just sn }) = sn
securityNature sec
    |      redeemable sec && Maybe.isJust    (term sec)      = Debt
    |      redeemable sec && Maybe.isNothing (term sec)      = Preferred
    |      redeemable sec && any preferredRight (rights sec) = Preferred
    | not (redeemable sec)                                   = Ordinary
    | otherwise                                              = Unknown
   where
    preferredRight (Voting 1)     = False
    preferredRight (Voting 0)     = False
    preferredRight (Voting _)     = True
    preferredRight Board          = True
    preferredRight (OtherRight _) = True

instance NL.Lang SecurityNature where
  to lang@(NL.Ctx { NL.lang=NL.English }) Ordinary = "ordinary shares"
  to lang@(NL.Ctx { NL.lang=NL.English }) Preferred = "preferred shares"
  to lang@(NL.Ctx { NL.lang=NL.English }) Debt = "debt"
  to lang@(NL.Ctx { NL.lang=NL.English }) Equity = "equity"
  to lang@(NL.Ctx { NL.lang=NL.English }) Unknown = "UNKNOWN"


