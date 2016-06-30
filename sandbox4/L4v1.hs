
-- first draft
module L4v1 where

data Agreement = Agreement { clauses :: [Clause]
                           , effectiveDate :: EffectiveDate
                           , parties :: [Party]
                           }
instance Show Agreement where
  show a = show $ effectiveDate a

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

data After = Immediately | After Date
instance Show After where
  show Immediately = "Immediately this clause comes into effect"
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
            | Breach
instance Show Clause where
  show Fulfilled = "the clause is trivially fulfilled"
  show Breach    = "the clause is breached"
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

instance Show Condition where
  show (Payment toParty currency amount) = "pay " ++ toParty ++ " " ++ currency ++ " " ++ show amount
  show (Gesture g)                       = "announce " ++ g

describeAction :: Condition -> String
describeAction (Payment _ _ _) = "making a payment"
describeAction (Gesture _)     = "making some public gesture"

