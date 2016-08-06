module SGConstitution where

import qualified Data.Time as Time

-- actors in deontic clauses are entities

-- todo: create a class of identifiableEntity
--                             , entityIdType :: Maybe String
--                             , entityIdVal :: Maybe String
--                             , entityJurisdiction :: Maybe String -- country

data Director    = Director    Person            deriving (Show)
data Shareholder = Shareholder Person ShareClass            deriving (Show)
data Investor    = Investor    Person Security            deriving (Show)

type Board        = [Director]
type Shareholders = [Shareholder]
type Investors    = [Investor]

data Person = Company { board        :: Board
                      , shareholders :: Shareholders
                      , entityName   :: String
                      }
            | Individual  { entityName :: String }
            | OtherPerson { entityName :: String }
            deriving (Show)

data AbstractPerson = AbstractCompany
                    | AbstractIndividual
                    | AbstractOtherPerson
                    deriving (Show)

-- a share class can be Ordinary or Other
data ShareClass = Ordinary
                | Preferred { shareClassName :: String
                            , rights :: String
                            }
                deriving (Show)              

data Entity a = Entity Person
              | EntityGroup a
              | AbstractEntity AbstractPerson
              | AbstractEntityGroup a
              deriving (Show)              

-- later we'll load some other module that knows more about securities
data Security = Security String
              deriving (Show)

-- the basic deontic primitives; we note that Must and MustNot are opposites
data Deontic = Must
             | May
             | MustNot
             deriving (Show)

-- some notion of time
data Temporal = After TimeRef
              | Before TimeRef
              | During Temporal Temporal
              | Intervals [Temporal]
              deriving (Show)

-- a time can be measured relative to an event, or an absolute time
data TimeRef = RelativeTime EventTime
             | AbsoluteTime Time.UTCTime
             deriving (Show)

-- relative to an event
data EventTime = EventStart Event
               | EventEnd   Event
               deriving (Show)

-- a description of some event that happened or will happen
data Event = Event String Action
            deriving (Show)

-- acording to Hvitved, a clause is made up of an action, which is parameterized
data Action = Performative PerformativeStatement
            | Deliver Deliverable
            | SendNotice NoticeMessage
            | IssueSecurity Security
            deriving (Show)

type PerformativeStatement = String
type Deliverable = String
type NoticeMessage = String

data Condition = Always
               | Never
               | CondIf Predicate

evalCondition :: Condition -> Bool
evalCondition Always = True
evalCondition Never  = False
evalCondition _      = True

showCond :: Condition -> String
showCond Always = "always"
showCond Never  = "never"
showCond cond   = "some condition goes here"

data Predicate = PredAnd [Predicate]
               | PredOr  [Predicate]
               | PredTemporal (Temporal -> Bool)
               | PredEvent    (Event    -> Bool)
               | PredOther    (String   -> Bool) -- ...

-- a rough equivalent to a clause, only in legislation this is a constraint
-- this does not mean "unask the question"
data Mu e = Mu { entity   :: Entity e
               , cond     :: Condition
               , deontic  :: Deontic
               , action   :: Action
               , temporal :: Temporal
               , thence   :: Mu e
               , lest     :: Mu e
               }
        | AndMu [Mu e]
        |  OrMu [Mu e]
        | TriviallyFulfilled
        | TriviallyBreached

showMu :: (Show e) => (Mu e) -> String
showMu mu = unlines [
  "If "   ++ showCond (cond mu),
  "then " ++ show (entity mu),
  show (deontic mu),
  show (action mu),
  show (temporal mu)
  ]
  


{-
8.—(1)  If at any time the share capital is divided into different classes of shares, the rights attached to any class (unless otherwise provided by the terms of issue of the shares of that class) may, whether or not the company is being wound up, be varied with —
(a)
the consent in writing of the holders of 75% of the issued shares of that class; or
(b)
the sanction of a special resolution passed at a separate general meeting of the holders of the shares of the class.
(2)  The provisions of this Constitution relating to general meetings apply with the necessary modifications to every separate general meeting of the holders of the shares of the class referred to in paragraph (1), except that —
(a)
the necessary quorum is at least 2 persons holding or representing by proxy one-third of the issued shares of the class; and
(b)
any holder of shares of the class present in person or by proxy may demand a poll.
(3)  Section 184 of the Act applies with the necessary modifications to every special resolution passed at a separate general meeting of the holders of the shares of the class under paragraph (1)
-}



{-
45.—(1)  Subject to any direction to the contrary that may be given by the company in general meeting, all new shares must, before issue, be offered to all persons who, as at the date of the offer, are entitled to receive notices from the company of general meetings, in proportion, or as nearly as the circumstances admit, to the amount of the existing shares to which they are entitled.
(2)  The offer must be made by notice specifying the number of shares offered, and limiting a time within which the offer, if not accepted, is treated to be declined.
(3)  After the expiration of the time referred to in paragraph (2), or upon the person to whom the offer is made declining the shares offered, the directors may dispose of those shares in any manner as they think is the most beneficial to the company.
(4)  The directors may dispose of any new shares which (by reason of the ratio which the new shares bear to shares held by persons entitled to an offer of new shares) cannot, in the opinion of the directors, be conveniently offered under this regulation.
-}
