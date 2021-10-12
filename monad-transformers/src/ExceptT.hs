--------------------------------------------------------------------------------
-- Functional Programming II
-- Lecture: Monad transformers
--------------------------------------------------------------------------------
-- Copyright (c) 2021 Michael B. Gale (michael@fpclass.online)
-- 
-- This source code is subject to the copyright notice found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

module ExceptT where 

--------------------------------------------------------------------------------

newtype ExceptT e m a = MkExceptT (m (Either e a))

instance Functor f => Functor (ExceptT e f) where 
    fmap f (MkExceptT m) = MkExceptT (fmap (fmap f) m)

instance Applicative m => Applicative (ExceptT e m) where 
    pure x = MkExceptT (pure $ Right x)

    (MkExceptT m0) <*> (MkExceptT m1) =
        MkExceptT (fmap (<*>) m0 <*> m1)

instance Monad m => Monad (ExceptT e m) where
   (MkExceptT m) >>= f = MkExceptT $ do
       e <- m
       case e of 
           Left err -> pure $ Left err
           Right val -> let (MkExceptT m0) = f val
                        in m0

throwE :: Monad m => e -> ExceptT e m a 
throwE = MkExceptT . pure . Left

runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT (MkExceptT m) = m

--------------------------------------------------------------------------------
