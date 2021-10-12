--------------------------------------------------------------------------------
-- Functional Programming II
-- Lecture: Monad transformers
--------------------------------------------------------------------------------
-- Copyright (c) 2021 Michael B. Gale (michael@fpclass.online)
-- 
-- This source code is subject to the copyright notice found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

module ReaderT where 

--------------------------------------------------------------------------------

newtype ReaderT s m a = MkReaderT (s -> m a)

instance Functor m => Functor (ReaderT s m) where
    fmap f (MkReaderT m) =
         MkReaderT $ \s -> fmap f (m s)

instance Applicative m => Applicative (ReaderT s m) where
    pure x = MkReaderT $ \_ -> pure x

    (MkReaderT m0) <*> (MkReaderT m1) =
         MkReaderT $ \s -> m0 s <*> m1 s

instance Monad m => Monad (ReaderT s m) where
    (MkReaderT m) >>= f =
        MkReaderT $ \s -> m s >>= \x ->
            let (MkReaderT m1) = f x 
            in m1 s

runReaderT :: ReaderT s m a -> s -> m a 
runReaderT (MkReaderT m) s = m s

--------------------------------------------------------------------------------
