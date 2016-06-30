
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
data Action = Payment | Gesture
instance Show Action where
  show Payment = "payment transaction"
  show Gesture = "some performative statement "
type FromParty = Party
type ToParty = Party
data ToBool = ToBool (Agreement -> Temporal -> Bool)

data After = Immediately | After Date
instance Show After where
  show Immediately = "immediately"
  show (After date) = "after " ++ date

data Condition = SatisfyingPayment FromParty ToParty String Int
               | SatisfyingGesture FromParty
instance Show Condition where
  show (SatisfyingPayment fromParty toParty currency amount)
    = fromParty ++ " pays " ++ toParty ++ " " ++ currency ++ show amount
  show (SatisfyingGesture fromParty)
    = fromParty ++ " makes some public gesture"
data Clause = Clause { precondition :: ToBool
                     , responsibleParty :: Party
                     , action :: Action
                     , condition :: Condition
                     , after :: After
                     , within :: DateInterval
                     , consequent :: Clause
                     , reparation :: Clause
                     }
            | Fulfilled
            | Breach

instance Show Clause where
  show Fulfilled = "the clause is trivially fulfilled"
  show Breach    = "the clause is breached"
  show Clause { precondition = precondition
              , responsibleParty = responsibleParty
              , action = action
              , condition = condition
              , after = after
              , within = within
              , consequent = consequent
              , reparation = reparation
              }
    = responsibleParty ++ " is responsible" ++
    " for performing a " ++ show action ++
    " satisfying the condition " ++ show condition ++
    " " ++ show after ++
    " within " ++ show within ++ " seconds" ++
    ". " ++
    "If this clause is performed, then " ++ show consequent ++
    ", but if it is not, then " ++ show reparation ++
    "."

