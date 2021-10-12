--------------------------------------------------------------------------------
-- Functional Programming II
-- Lecture: Monad transformers
--------------------------------------------------------------------------------
-- Copyright (c) 2021 Michael B. Gale (michael@fpclass.online)
-- 
-- This source code is subject to the copyright notice found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

module ExampleStack where

--------------------------------------------------------------------------------

import Prelude hiding (lookup)
import qualified Data.List as L

import Academic
import Identity
import ReaderT
import ExceptT

--------------------------------------------------------------------------------

lookup 
    :: Monad m
    => String 
    -> ReaderT [(String, String)] (ExceptT String m) String
lookup name = MkReaderT $ \dict ->
    case L.lookup name dict of
        Nothing -> throwE $ "Can't find " ++ name
        Just val -> pure val

fromDictionary 
    :: Monad m 
    => ReaderT [(String, String)] (ExceptT String m) Academic
fromDictionary = do
    name <- lookup "name"
    office <- lookup "office"
    title <- lookup "title" 
    pure $ Academic name office title

list :: [(String, String)]
list = [ ("name", "Monad Monoidson")
       , ("office", "At home")
       , ("title", "Professor") ]

--------------------------------------------------------------------------------
