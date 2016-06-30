
-- first draft
module L4v1 where

data Agreement = Agreement { clauses :: [Clause]
                           , effectiveDate :: EffectiveDate
                           , agreementDate :: Date
                           , parties :: [Party]
                           }
instance Show Agreement where
  show a = (preamble a) ++ agreementDate a ++ "\n"
    ++ dashify (parties a)
    ++ "(the \"Parties\")\n\nhereby agree as follows:\n\n"
    ++ show (effectiveDate a) ++ ",\n\n"
    ++ joinclauses (map upgradeTopLevel (clauses a))

-- upgrade the top-level clauses to be effective upon contract
-- rather than effective upon entry into the clause
upgradeTopLevel :: Clause -> Clause
upgradeTopLevel = upgradeImmediately . upgradeConsequents

upgradeImmediately :: Clause -> Clause
upgradeImmediately c
  | after c == Immediately = c { after = ImmediatelyContract }
  | otherwise              = c

upgradeConsequents :: Clause -> Clause
upgradeConsequents c = c { reparation = newreparation }
  where
    newreparation = if reparation c == Breach
                    then BreachContract    else reparation c

--
-- output layout sugar
--

joinclauses :: [Clause] -> String
joinclauses = concat . map (\c -> show c ++ "\n\n")

dashify :: [Party] -> String
dashify = concat . map (\p -> "- " ++ p ++ "\n")

--
-- preamble
-- if an agreement doesn't include consideration then we word it as a deed.
-- 
preamble :: Agreement -> String
preamble a
  | hasConsideration = "This agreement dated "
  | otherwise        = "Know all men by these presents, that as of "
    where hasConsideration = any (\clause ->
                                   isPayment (condition clause)) (clauses a)

data EffectiveDate = UponSignature | UponDate Date
instance Show EffectiveDate where
  show UponSignature = "Effective immediately upon signature by all parties"
  show (UponDate date) = "Effective " ++ date

data Temporal = TemporalDate Date | TemporalEvent Event
data Event = Ready
type Date = String
type DateInterval = Int
type Party = String
type FromParty = Party
type ToParty = Party
data ToBool = ToBool (Agreement -> Temporal -> Bool)
instance Eq ToBool where
  a == b = True
  
data After = Immediately | ImmediatelyContract | After Date
             deriving (Eq)
instance Show After where
  show Immediately = "Immediately this clause comes into effect"
  show ImmediatelyContract = "Upon the effective date of this contract"
  show (After date) = "After " ++ date

data Clause = Clause { precondition :: ToBool
                     , responsibleParty :: Party
                     , after :: After
                     , within :: DateInterval
                     , consequent :: Clause
                     , reparation :: Clause
                     , condition :: Condition
                     }
            | Fulfilled
            | Breach    | BreachContract
              deriving (Eq)
instance Show Clause where
  show Fulfilled = "the clause is fulfilled"
  show Breach    = "the clause is breached"
  show BreachContract    = "the contract is breached"
  show Clause { precondition = precondition
              , responsibleParty = responsibleParty
              , after = after
              , within = within
              , consequent = consequent
              , reparation = reparation
              , condition = condition
              }
    = responsibleParty ++ " is responsible for "
    ++ describeAction condition ++ ". "
    ++ show after ++ ", "
    ++ responsibleParty ++ " has " ++ show within ++ " seconds"
    ++ " to " ++ show condition ++ ". "
    ++ "If this clause is performed, then " ++ show consequent
    ++ ", but if it is not, then " ++ show reparation
    ++ "."

data Condition = Payment ToParty String Int
               | Gesture String
                 deriving (Eq)

instance Show Condition where
  show (Payment toParty currency amount) = "pay " ++ toParty ++ " " ++ currency ++ " " ++ show amount
  show (Gesture g)                       = "announce " ++ g

describeAction :: Condition -> String
describeAction (Payment _ _ _) = "making a payment"
describeAction (Gesture _)     = "making some public gesture"

isPayment :: Condition -> Bool
isPayment (Payment _ _ _) = True
isPayment (Gesture _)     = False

