--------------------------------------------------------------------------------
-- Functional Programming II
-- Lecture: Monad transformers
--------------------------------------------------------------------------------
-- Copyright (c) 2021 Michael B. Gale (michael@fpclass.online)
-- 
-- This source code is subject to the copyright notice found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

module Identity where

--------------------------------------------------------------------------------

newtype Identity a = MkIdentity a

instance Functor Identity where
    fmap f (MkIdentity x) = MkIdentity (f x)

instance Applicative Identity where
    pure = MkIdentity

    (MkIdentity f) <*> (MkIdentity x) = MkIdentity (f x)

instance Monad Identity where
   (MkIdentity x) >>= f = f x

runIdentity :: Identity a -> a 
runIdentity (MkIdentity x) = x

--------------------------------------------------------------------------------
