--------------------------------------------------------------------------------
-- Functional Programming II
-- Lecture: Monad transformers
--------------------------------------------------------------------------------
-- Copyright (c) 2021 Michael B. Gale (michael@fpclass.online)
-- 
-- This source code is subject to the copyright notice found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

module Problem where

--------------------------------------------------------------------------------

import ExampleStack

import Academic
import Identity
import ReaderT
import ExceptT
import MonadTrans

--------------------------------------------------------------------------------

loadAndCheck 
    :: Monad m 
    => ReaderT [(String, String)] (ExceptT String m) Academic
loadAndCheck = do
    academic <- fromDictionary

    if academicName academic /= "Lord Monad"
    then lift $ throwE "Only functional programmers allowed!"
    else pure academic

--------------------------------------------------------------------------------
