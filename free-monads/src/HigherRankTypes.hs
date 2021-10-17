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

module HigherRankTypes where

--------------------------------------------------------------------------------

-- This does not typecheck without the explicit type signature 
-- and RankNTypes enabled. Note that there are other permissable types:
-- (forall a. a -> Int) -> (Int, Int)
-- (forall a. a -> Bool) -> (Bool, Bool)
-- etc.
foo :: (forall a. a -> a) -> (Bool, Char)
foo f = (f True, f 'x') 

--------------------------------------------------------------------------------
