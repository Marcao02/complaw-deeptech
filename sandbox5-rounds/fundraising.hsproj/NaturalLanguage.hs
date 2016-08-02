
module NaturalLanguage where

import Data.Maybe

import qualified Data.Time.Calendar as Calendar

data Tongue  = English | Italian | French | Bahasa    deriving (Show)
data Tense = Past | Present | Future       deriving (Show)

data Ctx = Ctx { lang :: Tongue
               , tense :: Maybe Tense
               , refTime :: Calendar.Day}
         deriving (Show)

v :: String -> Ctx -> String
v str ctx = v' str (lang ctx) (fromMaybe Present $ tense ctx)

v' :: String -> Tongue -> Tense -> String

v' "raise" English Present = "raising"
v' "raise" English Future  = "will raise"
v' "raise" English Past    = "raised"

v' "is" English Present = "is"
v' "is" English Future  = "will be"
v' "is" English Past    = "was"

class Lang a where
  to    :: Ctx -> a -> String
--
-- Named: nameOf
--

class Named thing       where nameOf :: thing -> String

