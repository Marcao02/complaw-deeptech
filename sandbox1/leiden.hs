

import Data.String
import Data.Map
import Data.List

class English a where
  en_SG :: a -> String

-- *** breach_consequence
-- BC ::= intensional_consequence+ | extensional_consequence+ | breach_exception
-- intensional_consequence ::= party.attribute_change()
-- extensional_consequence ::= TE
-- breach_exception ::= monad

data BC = Intensional String
        | ExtensionalTE [TE]
        | ExtensionalCE [CE]
        | BreachException

instance Show BC where
  show (Intensional c) = "suffers an intensional consequence: " ++ c
  show (ExtensionalTE tes) = "extensional consequence: " ++ show tes
  show (ExtensionalCE ces) = "extensional consequence: " ++ show ces
  show BreachException = "generic breach exception"

data Context = Context { primary :: Party,
                         counter :: Party,
                         oe      :: OE }

instance English BC where
  en_SG (Intensional a) = "primary party suffers intensional consequence" ++ a
  en_SG (ExtensionalTE []) = ""
  en_SG (ExtensionalTE (te:tes)) = "real consequences follow timely: " ++ en_SG te ++ "\n" ++ en_SGtes tes
  en_SG (ExtensionalCE []) = ""
  en_SG (ExtensionalCE ces) = "real conditional consequences follow: " ++ dashList ces
  en_SG BreachException = "go see a judge"

-- 
-- *** obligation expression
-- OE ::= ( must | may | mustnot ) (action+ lest BC+)+
-- action ::= perform | send_notice
-- perform ::= act_monad | hereby_statement | procurement
-- send_notice ::= party sends notice to some other party
-- procurement ::= procure PE

data OE = Must     ([Action], [BC])
        | May        Action
        | MustNot  ([Action], [BC])
        deriving (Show)

dashList a = concat $ Data.List.map ("  - " ++)
                    $ Data.List.map en_SG a

instance English OE where
  en_SG (Must (actionlist, breachlist)) =
     "the responsible party MUST perform each of the actions listed in the action list below:" ++ (dashList actionlist)
     ++ "\n\n... if the responsible party is irresponsible, then we go to breach state:\n" ++ (dashList breachlist)
  en_SG (May action) = "the responsible party MAY proceed to " ++ en_SG action ++ ";"
  en_SG (MustNot (actionlist, breachlist)) =
     "the responsible party MUST NOT perform any of the actions listed in the action list below:" ++ (dashList actionlist)
     ++ "\n\n... if they do, then we go to breach state:\n" ++ (dashList breachlist)

data Action = Perform String
            | SendNotice Notice
            | Procure Procurement
            deriving (Show)

instance English Action where
  en_SG (Perform a) = "perform " ++ a
  en_SG (SendNotice a) = "send notice " ++ en_SG a
  en_SG (Procure a) = "procure" ++ en_SG a

data Notice = Notice Party String
            deriving (Show)

instance English Notice where
  en_SG (Notice p s) = "send notice to party " ++ en_SG p ++ ": " ++ s

data Procurement = Procurement Party String
                 deriving (Show)

instance English Procurement where
  en_SG (Procurement p s) = "procure that party " ++ en_SG p ++ " do: " ++ s

-- 
-- *** party expression
-- PE ::= party OE+
-- party ::= individual | corporation

data PE = PE Party [OE]
        deriving (Show)

instance English PE where
  en_SG (PE party oes) = "party expression primary="++en_SG party++" has " ++ (show $ length oes) ++ " obligation expressions, as follows:" ++ (concat $ Data.List.map en_SG oes)

data Party = Individual PartyName PartyID
           | Corp       PartyName PartyID
           deriving (Show, Ord, Eq)
           
instance English Party where
  en_SG (Individual p (pidtype, pid)) = "individual party " ++ p ++ " (" ++ pidtype ++ ": " ++ pid ++ ")"
  en_SG (Corp       p (pidtype, pid)) = "corporate party "  ++ p ++ " (" ++ pidtype ++ ": " ++ pid ++ ")"
                 
type PartyName = String
type PartyID   = (String,String)

-- 
-- *** temporal expression
-- TE ::= temporal PE+
-- temporal ::= ( at | upon | when | while | before | after | from ) datetime
--

data TE = TE Temporal [PE]
          deriving (Show)

instance English TE where
  en_SG (TE temporal pes) = en_SG temporal ++ (concat $ Data.List.map en_SG pes)

data Temporal = At     Whenever
              | Upon   Whenever
              | When   Whenever
              | While  Whenever
              | Before Whenever
              | After  Whenever
              | From   Whenever
          deriving (Show)
instance English Temporal where
  en_SG (At whenever) = "Temporal at " ++ whenever
  en_SG _ = "Temporal generic"
  
type Whenever = String

-- *** conditional expression
-- CE ::= case ( ifpredicate TE+ )* default TE*
-- ifpredicate ::= condition_monad
-- 

data CE = CE [(CondIf,TE)] [TE]
  deriving (Show)

instance English CE where
  en_SG (CE citetes tes) = (concat $ Data.List.map (\a -> "IFCONDITION: " ++ en_SG (fst a) ++ " TEMPORAL: " ++ en_SG (snd a)) citetes) ++ "\notherwise, " ++ (en_SGtes tes)


en_SGtes :: [TE] -> String
en_SGtes [] = ""
en_SGtes tes = concat $ Data.List.map (("  - " ++) . en_SG) tes


data CondIf = CondIf Scenario
  deriving (Show)

instance English CondIf where
  en_SG (CondIf scenario) = "if the scenario is true: " ++ (en_SG scenario)
  
condif :: Scenario -> Bool
condif scenario = True

data Scenario = Scenario [Event]
  deriving (Show)

instance English Scenario where
  en_SG (Scenario evlist) = "Scenario: " ++ (concat $ Data.List.map ("  - " ++) $ Data.List.map en_SG evlist)
  
data Event = Event (Maybe Whenever) String (Maybe Whenever)

instance English Event where
  en_SG (Event a b c) = show (Event a b c)

instance Show Event where
  show (Event Nothing b Nothing) = b ++ " has always been, and will always be."
  show (Event (Just a) b Nothing) = b ++ " started at " ++ a ++ " and will forever endure."
  show (Event Nothing b (Just c)) = b ++ " has always happened, but will come to an end at " ++ c
  show (Event (Just a) b (Just c)) = b ++ " began at " ++ a ++ " and will end at " ++ c

-- *** consideration
-- we put that into an execution expression
-- which includes the conditions precedent
-- 
data EE = EE Consideration ConditionsPrecedent Parties [CE]
  deriving (Show)

instance English EE where
  en_SG (EE consideration cp parties ces) = "Dated This Day (date), this agreement between the parties\n" ++ (en_SG parties) ++ "\n\nwitnesseseth as follows:\n" ++ en_SG consideration ++ " upon the verity of the following conditions precedent:\n" ++ en_SG cp ++ "\n\nSO BASICALLY WHAT WILL HAPPEN IS THIS:\n" ++ (concat $ Data.List.map en_SG ces)

data Parties = Parties [Party]
  deriving (Show)
  
instance English Parties where
  en_SG (Parties ps) = concat $ Data.List.map (++"\n") $ Data.List.map en_SG ps
  
data Consideration = Consideration Party Payment Party
  deriving (Show)

instance English Consideration where
  en_SG (Consideration p1 payment p2) = en_SG p1 ++ " shall pay " ++ en_SG payment ++ " to " ++ en_SG p2
  
data Payment = Payment { currency :: String, amount :: Int }
  deriving (Show)

instance English Payment where
  en_SG (Payment { currency=pcurr, amount=pamount } ) = pcurr ++ (show pamount)

data ConditionsPrecedent = ConditionsPrecedent [CondIf]
  deriving (Show)

instance English ConditionsPrecedent where
  en_SG (ConditionsPrecedent []) = "(no conditions precedent)"
  en_SG (ConditionsPrecedent condifs) = "each of the following is true:\n" ++ (concat $ Data.List.map en_SG condifs)
  




