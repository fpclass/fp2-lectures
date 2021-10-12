--------------------------------------------------------------------------------
-- Functional Programming II
-- Lecture: Monad transformers
--------------------------------------------------------------------------------
-- Copyright (c) 2021 Michael B. Gale (michael@fpclass.online)
-- 
-- This source code is subject to the copyright notice found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

module MonadTrans where

--------------------------------------------------------------------------------

import ExceptT
import ReaderT

class MonadTrans t where
    lift :: Monad m => m a -> t m a

instance MonadTrans (ExceptT e) where
    lift = MkExceptT . fmap Right

instance MonadTrans (ReaderT s) where
    lift = MkReaderT . const

--------------------------------------------------------------------------------
