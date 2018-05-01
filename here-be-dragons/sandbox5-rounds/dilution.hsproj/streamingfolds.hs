{-# LANGUAGE ExistentialQuantification, InstanceSigs, ScopedTypeVariables, KindSignatures, GADTs, TypeOperators #-}

import Data.List (foldl')
import Data.Monoid

data Fold (a :: *) (b :: *) = forall (w :: *) .  (Monoid w) => Fold
    { tally     :: a -> w
    , summarize :: w -> b
    }
                            
fold :: Fold a b -> [a] -> b
fold (Fold t c) xs =
    c (foldl' mappend mempty (map t xs))

{-
fold (Fold (\a -> Times a) (\b -> getTimes b `div` 2 == 0)) [1,2,3,4,5,6]
(\b -> getTimes b `div` 2 == 0) (foldl' mappend mempty (map (\a -> Times a) [1,2,3,4,5,6]))
(\b -> getTimes b `div` 2 == 0) ((foldl' mappend mempty [Times 1,Times 2,Times 3,Times 4,Times 5,Times 6]) :: Num a => Times a)
(\b -> getTimes b `div` 2 == 0) ([Times 1 `mappend` Times 2 `mappend` Times 3 ...])
(\b -> getTimes b `div` 2 == 0) (Times 720)
(720 `mod` 2 == 0)
True
-}

-- let us do multiplication
      
newtype Times a = Times { getTimes :: a }
newtype Plus a = Plus { getPlus :: a }

instance Show a => Show (Times a) where
    show (Times a) = "Times " ++ show a

data Times2 :: * -> * where
  Times2 :: (a :: *) -> ((Times2 :: * -> *) (a :: *) :: *) 

instance Num a => Monoid (Times a) where
    mempty :: Times a
    mempty = Times 1
    mappend :: Times a -> Times a -> Times a
    mappend (Times x) (Times y) = Times (x * y)

instance Num a => Monoid (Plus a) where
    mempty :: Plus a
    mempty = Plus 0
    mappend :: Plus a -> Plus a -> Plus a
    mappend (Plus x) (Plus y) = Plus (x + y)

product :: Num a => Fold a a
product = Fold (Times :: Num a => Times a) getTimes
          -- Note that there exists an instance
          -- Num a  => Monoid (Times a)
          -- We map over some foldable 'Num a => a' with Times
          -- cat those together
          -- getting some other Num a => Times a
          -- And then apply getTimes :: Num a => Times a -> a
          

mysum :: Num a => Fold a a
mysum = Fold Plus getPlus

instance Functor (Fold a) where
  fmap  :: (b -> c) -> (Fold a b -> Fold a c)
  fmap f (Fold a b) = Fold a (f . b)
          
-- instance Applicative Fold where
-- pure
-- <*>


instance Applicative (Fold a) where
    pure :: b -> Fold a b
    pure b = Fold (\_ -> ()) (\_ -> b)
    (<*>) :: Fold a (b -> c) -> (Fold a b -> Fold a c)
    Fold  (a :: blah -> m1) (p :: m1 -> (poo -> oop)) <*>
     Fold (b :: blah -> m2) (q :: m2 ->  poo) =
        Fold ((\argh -> (a argh, b argh)) :: blah -> (m1,m2)) -- a
             ((\(m1, m2) -> p m1 $ q m2)  :: (m1,m2) -> oop)  -- c

-- poop is poo -> oop
-- barf needs to be poo

                            
    
{-             
 what does the product of two monoids look like?
 when two monoids love each other very much, they exchange rings and make a new monoid.

 if a plus (a b) meets a plus (c d),
 the mempty is 0 and the mappend is a + b + c + d

 if a plus meets a times,
 the mempty is

 if a times meets a times, 

 if a times meets a plus, 
-}



-- tally
-- -- for tf
-- -- for idf


-- summarize
-- -- for tf
-- -- for idf
