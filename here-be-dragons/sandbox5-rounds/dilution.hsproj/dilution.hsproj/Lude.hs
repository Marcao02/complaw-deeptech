{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}

module Lude where

undefined :: forall a. a
undefined = undefined

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
    return :: a -> f a
    return = pure
    (>>=) :: f a -> (a -> f b) -> f b

data Identity a = Identity a

instance Functor Identity where
    fmap = undefined

instance Applicative Identity where
    pure = undefined
    (<*>) = undefined

instance Monad Identity where
    (>>=) = undefined

data Zero a = Zero

instance Functor Zero where
    fmap = undefined

instance Applicative Zero where
    pure = undefined
    (<*>) = undefined

instance Monad Zero where
    (>>=) = undefined

data Const a e = Const a

instance Functor (Const a) where
    fmap = undefined

instance Applicative (Const a) where
    pure = undefined
    (<*>) = undefined

instance Monad (Const a) where
    (>>=) = undefined

data Maybe a = Just a | Nothing

instance Functor Maybe where
    fmap = undefined

instance Applicative Maybe where
    pure = undefined
    (<*>) = undefined

instance Monad Maybe where
    (>>=) = undefined


data Pair a b = a , b

-- masochism starts here
data Free f a = Pure a | Free (f (Free f a))

instance Functor (Free f) where
    fmap = undefined

instance Applicative (Free f) where
    pure = undefined
    (<*>) = undefined

instance Monad (Free f) where
    (>>=) = undefined
