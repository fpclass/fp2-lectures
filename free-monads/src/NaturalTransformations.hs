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

module NaturalTransformations where

--------------------------------------------------------------------------------

type f ~> g = forall a. f a -> g a

--------------------------------------------------------------------------------
