-- my first haskell program!
module Legalese where

data Money
data Time

data Message = Intend   [EventSpec]
             | Happened [EventSpec]
             | Invite Statement


data ShareType = Common

data FinancialInstrument = Shares ShareType
                         | Bonds

data Person = Person String

data Event = Ev { when :: Time
                , what :: EventType }

data EventSpec = Event Person EventType

data EventType = Pay         Money
                | Send     { adressee :: Person, msg :: Message }
                | Resolution Message
                | Issue      FinancialInstrument

data Statement = -- First order logic
                 Statement :&&: Statement -- and
               | Statement :||: Statement -- or
               | Statement :=>: Statement -- or
               | Not Statement
                 -- Temporal modalities
               | On  Time Statement
               | For Time Statement
                 -- Capability
               | May      Person Statement
               | Must     Person Statement
               | Happens  [EventSpec]
                 -- Epistemic
               | Believes Person Statement
               | Know     Person Statement -- ???

type EventStream = [Event]

check :: EventStream -> Statement -> Bool
check = undefined

proceed :: EventStream -> Person -> [Event]
proceed  = undefined

parseStory :: String -> EventStream
parseStory  = undefined

data Language = EN_SG
              | Etherium

generateContract :: Statement -> Language -> String
generateContract  = undefined

