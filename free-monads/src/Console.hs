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

module Console where

--------------------------------------------------------------------------------

import Prelude hiding (putStrLn, getLine)
import qualified Prelude (putStrLn, getLine)

import Control.Monad.Trans.State

import NaturalTransformations
import Free

--------------------------------------------------------------------------------

data ConsoleF a
    = PutStrLn String a
    | GetLine (String -> a)

instance Functor ConsoleF where
    fmap f (PutStrLn xs a) = PutStrLn xs (f a)
    fmap f (GetLine k) = GetLine (f . k)

type Console = Free ConsoleF

putStrLn :: String -> Console ()
putStrLn xs = liftF (PutStrLn xs ())

getLine :: Console String
getLine = liftF (GetLine id)

-- | `consoleIO` @stmt@ is a natural transformation which acts
-- as an interpreter which maps statements of type `ConsoleF` 
-- to computations in `IO`.
consoleIO :: ConsoleF ~> IO
consoleIO (PutStrLn xs r) = do
    Prelude.putStrLn xs
    pure r
consoleIO (GetLine k) = do
    xs <- Prelude.getLine
    pure (k xs)

program :: Console ()
program = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name)

runProgram :: IO ()
runProgram = interp consoleIO program

consolePure :: ConsoleF ~> State ([String], [String])
consolePure (PutStrLn xs r) = do
    (ins, outs) <- get
    put (ins, outs ++ [xs])
    pure r
consolePure (GetLine k) = do
    (ins, outs) <- get
    put (tail ins, outs)
    pure (k (head ins))

runTest :: [String] -> ([String], [String])
runTest ins = execState (interp consolePure program) (ins, [])

--------------------------------------------------------------------------------
