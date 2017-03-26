
-- let's have a bunch of monads, so we can explore how monads interact.
-- this will help me figure out how the compiler decides which monads to apply,
-- based on the inputs and outputs and the stuff on the inside.

import Control.Applicative
import Control.Monad

data Doubler a = Doubler a
               deriving Show
                        
instance Functor Doubler where
    fmap f (Doubler a) = Doubler (f $ a)

instance Applicative Doubler where
    pure x = Doubler x
    (<*>) (Doubler f) (Doubler a) = Doubler (f a)

instance Monad Doubler where
    (>>=) (Doubler x) f = f x
    return = Doubler

potato = Doubler 1

potato2 y = Doubler $ y * 2
potato3 y = Doubler $ y * 3

main :: IO ()
main = do 
  let p = potato2 4
  putStrLn (show p)
           
mybind :: (Monad m) => m a -> (a -> m b) -> m b
mybind x f = join (fmap f x)

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
  then [x, x*x]
  else []
            
              
