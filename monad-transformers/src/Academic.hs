--------------------------------------------------------------------------------
-- Functional Programming II
-- Lecture: Monad transformers
--------------------------------------------------------------------------------
-- Copyright (c) 2021 Michael B. Gale (michael@fpclass.online)
-- 
-- This source code is subject to the copyright notice found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

module Academic where 

data Academic = Academic {
    academicName :: String,
    academicRoom :: String,
    academicTitle :: String
} deriving (Eq, Show)

--------------------------------------------------------------------------------
