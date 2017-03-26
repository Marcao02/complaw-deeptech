{-# LANGUAGE ExistentialQuantification, InstanceSigs #-}

import Data.List (foldl')
import Data.Monoid

data Fold a b = forall w .  (Monoid w) => Fold
    { tally     :: a -> w
    , summarize :: w -> b
    }

fold :: Fold a b -> [a] -> b
fold (Fold t c) xs =
    c (foldl' mappend mempty (map t xs))

-- let us do multiplication
      
newtype Times a = Times { getTimes :: a }

instance Num a => Monoid (Times a) where
    mempty :: Times a
    mempty = Times 1
    mappend :: Times a -> Times a -> Times a
    mappend (Times x) (Times y) = Times (x * y)

product :: Num a => Fold a a
product = Fold Times getTimes

instance Functor (Fold a) where
  fmap  :: (b -> c) -> (Fold a b -> Fold a c)
  fmap f (Fold a b) = Fold a (f . b)
          
-- instance Applicative Fold where
-- pure
-- <*>
    
-- tally
-- -- for tf
-- -- for idf


-- summarize
-- -- for tf
-- -- for idf
