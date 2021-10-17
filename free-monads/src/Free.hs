--------------------------------------------------------------------------------
-- Functional Programming II
-- Lecture: Free monads
--------------------------------------------------------------------------------
-- Copyright (c) 2021 Michael B. Gale (michael@fpclass.online)
-- 
-- This source code is subject to the copyright notice found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Free where

--------------------------------------------------------------------------------

import NaturalTransformations

--------------------------------------------------------------------------------

data Free f a
    = Pure a
    | Free (f (Free f a))

instance Functor f => Functor (Free f) where
    -- fmap :: (a -> b) -> Free f a -> Free f b
    fmap g (Pure x) = Pure (g x)
    fmap g (Free fx) = Free (fmap (fmap g) fx)

instance Functor f => Applicative (Free f) where
    -- pure :: a -> Free f a
    pure = Pure

    -- (<*>) :: Free f (a -> b) -> Free f a -> Free f b
    (<*>) (Pure f) (Pure x) = Pure (f x)
    (<*>) (Pure f) fx = fmap f fx
    (<*>) (Free fx) m = Free (fmap (<*> m) fx)

instance Functor f => Monad (Free f) where
    -- (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    (>>=) (Pure x) k = k x
    (>>=) (Free fx) k = Free (fmap (>>= k) fx)

liftF :: Functor f => f a -> Free f a
liftF fa = Free (fmap Pure fa)

monad :: Monad m => Free m ~> m
monad (Pure x) = pure x
monad (Free fx) = fx >>= monad

freeM 
    :: (Functor f, Functor g)
    => f ~> g
    -> Free f ~> Free g
freeM phi (Pure x) = Pure x
freeM phi (Free fx) = Free (phi (fmap (freeM phi) fx))

interp 
    :: (Functor f, Monad m)
    => f ~> m
    -> Free f ~> m
interp phi = monad . freeM phi

--------------------------------------------------------------------------------
