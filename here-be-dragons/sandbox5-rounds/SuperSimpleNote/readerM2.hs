import Control.Monad.Reader
import Data.Map as Map
import Data.Maybe
import Control.Applicative

type Bindings = Map String Int;

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader calc_isCountCorrect bindings

-- The Reader monad, which implements this complicated check.
calc_isCountCorrect :: Reader Bindings Bool
calc_isCountCorrect = do
    count <- asks (lookupVar "count")
    bindings <- ask
    return (count == (Map.size bindings))

-- The selector function to  use with 'asks'.
-- Returns value of the variable with specified name.
lookupVar :: String -> Bindings -> Int
lookupVar name bindings = fromJust (Map.lookup name bindings)

sampleBindings :: Bindings
sampleBindings = Map.fromList [("count",3), ("1",1), ("b",2)]

main = do
    putStr $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": ";
    putStrLn $ show (isCountCorrect sampleBindings);

newtype HumanName = HumanName String
    deriving (Eq, Show)

newtype DogName = DogName String
    deriving (Eq, Show)

newtype Address = Address String
    deriving (Eq, Show)

data Person =
    Person {
      humanName :: HumanName
    , dogName   :: DogName
    , address   :: Address
    } deriving (Eq, Show)

data Dog =
    Dog {
      dogsName    :: DogName
    , dogsAddress :: Address
    , humansName  :: HumanName
    } deriving (Eq, Show)
            
pers  = Person (HumanName "Big Bird")    (DogName "Barkley") (Address "Sesame Street")
chris = Person (HumanName "Chris Allen") (DogName "Papu")    (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p) (humanName p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address <*> humanName

getDogR' :: Person -> Dog
getDogR' = liftA3 Dog dogName address humanName
          
myLiftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
myLiftA3 f a b c = f <$> a <*> b <*> c
