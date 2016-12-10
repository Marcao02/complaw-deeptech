{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) = (\(Identity a) -> Identity (f a))

instance Monad Identity where
    (Identity a) >>= f = f a

data Zero a = Zero

instance Functor Zero where
    fmap _ _ = Zero

instance Applicative Zero where
    pure _ = Zero
    Zero <*> Zero = Zero

instance Monad Zero where
    Zero >>= f = Zero

data Const a e = Const a

instance Functor (Const a) where
    fmap f (Const a) = Const a

class Monoid m where
    mempty :: m
    mappend :: m -> m -> m

instance Monoid a => Applicative (Const a) where
    pure :: b -> (Const a) b
    pure _ = Const mempty
    (<*>) :: Const a (b -> c) -> (Const a b -> Const a c)
    Const (l :: a) <*> Const (r :: a) = Const (mappend l r)

-- 1. Const mempty <*> v@(Const v') = Const (mappend mempty v') = Const v' = v
-- 2. ((pure (.) <*> u) <*> v) <*> w = u <*> (v <*> w) = ((Const mempty <*> u) <*> v) <*> w = (u <*> v) <*> w == u <*> (v <*> w)
-- 3. pure f <*> pure x = pure (f x) = Const mempty <*> Const mempty = Const (mempty <*> mempty) = (pure _ = Const mempty)
-- 4. u <*> pure y = pure ($ y)  <*> u = u <*> Const mempty = Const mempty <*> u == u == u


data Maybe a = Just a | Nothing

instance Functor Maybe where
    fmap _ Nothing = Nothing -- zero
    fmap f (Just a) = Just (f a) -- Identity

instance Applicative Maybe where
--    pure _ = Nothing -- the Neverending Story scenario
    pure = Just
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just f <*> Just x = Just (f x)
                    
instance Monad Maybe where
    Just x >>= f = f x
    Nothing >>= f = Nothing

data Pair a b = MkPair a b

instance Functor (Pair ppp) where
    fmap :: (aaa -> bbb) -> Pair ppp aaa -> Pair ppp bbb
    fmap f (MkPair p a) = (MkPair p) (f a)

instance Monoid a => Applicative (Pair a) where
    pure x = MkPair a x
        where a = mempty
    (MkPair (a :: a) (f :: b -> c)) <*> (MkPair (b :: a) (x :: b)) = MkPair (a `mappend` b) (f x)
    -- EXERCISE: Prove the following impls unlawful
    --
    -- (MkPair (a :: a) (f :: b -> c)) <*> (MkPair (b :: a) (x :: b)) = MkPair mempty (f x)
    -- (MkPair (a :: a) (f :: b -> c)) <*> (MkPair (b :: a) (x :: b)) = MkPair a (f x)
    -- (MkPair (a :: a) (f :: b -> c)) <*> (MkPair (b :: a) (x :: b)) = MkPair b (f x)
    -- (MkPair (a :: a) (f :: b -> c)) <*> (MkPair (b :: a) (x :: b)) = MkPair (b `mappend` a) (f x)



flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \b a -> f a b

id :: a -> a
id x = x      

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)   

class Bifunctor f where
    bimap :: (a -> b) -> (c -> d) -> f a c -> f b d
    bimap f g = first f . second g
    first :: (a -> b) -> f a c -> f b c
    first = flip bimap id
    second :: (c -> d) -> f a c -> f a d
    second = bimap id

instance Bifunctor Pair where
    first f (MkPair a b) = MkPair (f a) b
    second = fmap


