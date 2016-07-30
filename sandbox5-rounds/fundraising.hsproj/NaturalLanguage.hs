
module NaturalLanguage where

data Tongue  = English | Italian | French    deriving (Show)
data Tense = Past | Present | Future       deriving (Show)

data Ctx = Ctx { lang :: Tongue
               , tense :: Tense }
         deriving (Show)

class Lang a where
  to :: Ctx -> a -> String
