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

module Sum where

--------------------------------------------------------------------------------

import NaturalTransformations
import Free

--------------------------------------------------------------------------------

data Sum f g a
    = InL (f a)
    | InR (g a)

instance (Functor f, Functor g) => Functor (Sum f g) where
    fmap h (InL fa) = InL (fmap h fa)
    fmap h (InR ga) = InR (fmap h ga)

flattenSum :: (f ~> t) -> (g ~> t) -> Sum f g ~> t
flattenSum phi _ (InL fa) = phi fa
flattenSum _ psi (InR ga) = psi ga

left :: (Functor f, Functor g) => Free f ~> Free (Sum f g)
left = freeM InL

right :: (Functor f, Functor g) => Free g ~> Free (Sum f g)
right = freeM InR

--------------------------------------------------------------------------------
