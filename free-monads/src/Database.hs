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

module Database where

--------------------------------------------------------------------------------

import Control.Monad.Trans.Reader

import System.Environment

import NaturalTransformations
import Free

--------------------------------------------------------------------------------

data DatabaseF a
    = GetUserAge String (Maybe Int -> a)

instance Functor DatabaseF where
    fmap f (GetUserAge xs k) = GetUserAge xs (f . k)

type Database = Free DatabaseF

getUserAge :: String -> Database (Maybe Int)
getUserAge uname = liftF (GetUserAge uname id)

-- | `databaseIO` @stmt@ is a natural transformation which acts
-- as an interpreter which maps statements of type `DatabaseF` 
-- to computations in `IO`.
databaseIO :: DatabaseF ~> IO
databaseIO (GetUserAge xs k) = do
    age <- lookupEnv xs
    pure (k $ fmap read age)

databasePure :: DatabaseF ~> Reader [(String, String)]
databasePure (GetUserAge xs k) = do
    age <- fmap (lookup xs) ask
    pure (k $ fmap read age)

--------------------------------------------------------------------------------
