{-# LANGUAGE InstanceSigs #-}
import Control.Applicative
import Debug.Trace
    
hurr = (*2)
durr = (+10)

m :: Integer -> Integer
m = hurr . durr
w = fmap hurr durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr
m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

oof :: Integer -> Integer -> Integer
oof = (+)
boof :: Integer -> Integer -> Integer
boof = (*)

hurrDurr :: Integer -> Integer
hurrDurr = do
  a <- hurr
  b <- durr
  c <- (a * 2 +)
  return (b + c)

hurpDurp :: Integer -> Integer -> Integer
hurpDurp = do
  a <-  oof
  traceM "moo\n"
  b <- boof
  return (a . b)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader $ (f . ra)

-- i know that the type a is a partial function application (r ->), and so is a
ask :: Reader a a
ask = Reader $ id
      
