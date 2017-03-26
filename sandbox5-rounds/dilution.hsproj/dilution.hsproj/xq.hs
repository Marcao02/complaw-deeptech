{-# LANGUAGE ExistentialQuantification #-}


data Fold a b = forall w.  (Monoid w) => Fold
    { tally     :: a -> w
    , summarize :: w -> b
    }

fold :: Fold a b -> [a] -> b
fold (Fold t c) xs =
    c (foldl' mappend mempty (map t xs))


