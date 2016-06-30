
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
  show Immediately = "immediately"
  show (After date) = "after " ++ date

data Clause = Clause GenericC SpecificC
            | Fulfilled
            | Breach
instance Show Clause where
  show Fulfilled = "the clause is trivially fulfilled"
  show Breach    = "the clause is breached"
  show (Clause
        ( GenericC { precondition = precondition
                   , responsibleParty = responsibleParty
                   , after = after
                   , within = within
                   , consequent = consequent
                   , reparation = reparation
                   } )
        specificc)
    = responsibleParty ++ " is responsible for " ++
    describeAction specificc ++
    "; in this instance, " ++ responsibleParty ++ " must " ++
    show specificc ++
    "; and must do this " ++ show after ++
    " within " ++ show within ++ " seconds" ++
    ". " ++
    "If this clause is performed, then " ++ show consequent ++
    ", but if it is not, then " ++ show reparation ++
    "."

data GenericC = GenericC { precondition :: ToBool
                         , responsibleParty :: Party
                         , after :: After
                         , within :: DateInterval
                         , consequent :: Clause
                         , reparation :: Clause
                         }

data SpecificC = Payment ToParty String Int
               | Gesture String

instance Show SpecificC where
  show (Payment toParty currency amount)
    = "pay " ++ toParty ++ " " ++ currency ++ show amount
  show (Gesture g)
    = "announce " ++ g

describeAction :: SpecificC -> String
describeAction (Payment _ _ _) = "making a payment"
describeAction (Gesture _) = "making some public gesture"

