{-# LANGUAGE FlexibleInstances #-}

module Discourse where

import Control.Monad.Trans

data Post = Post { post_id :: Int }
    deriving Show

class MonadDiscourse m where
    getLatest :: m [Post]

instance MonadDiscourse IO where
    getLatest = error "not implemented MonadDiscourse IO.getLatest"

instance (Monad m, MonadDiscourse m, MonadTrans t) => MonadDiscourse (t m) where
    getLatest = lift getLatest
