--------------------------------------------------------------------------------
-- Functional Programming II
-- Lecture: Free monads
--------------------------------------------------------------------------------
-- Copyright (c) 2021 Michael B. Gale (michael@fpclass.online)
-- 
-- This source code is subject to the copyright notice found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

module Program where

--------------------------------------------------------------------------------

import Prelude hiding (putStrLn, getLine)

import Free
import Sum
import Console
import Database

--------------------------------------------------------------------------------

sumProgram :: Free (Sum ConsoleF DatabaseF) ()
sumProgram = do 
    left $ putStrLn "What is your name?"
    name <- left $ getLine
    age <- right $ getUserAge name

    case age of
        Nothing -> left $ putStrLn "I don't know how old you are"
        Just n -> left $ putStrLn ("You are " ++ show n ++ " years old!")

runSumProgram :: IO ()
runSumProgram = 
    interp (flattenSum consoleIO databaseIO) sumProgram

--------------------------------------------------------------------------------
