{-# LANGUAGE FlexibleInstances #-}

module Discourse where

import Control.Monad.Trans
import Data.Aeson.TH

data Post = Post { post_id :: Int }
    deriving Show

deriveJSON defaultOptions ''Post

class MonadDiscourse m where
    getLatest :: m [Post]

instance MonadDiscourse IO where
    getLatest = error "not implemented getLatest@IO"

instance (Monad m, MonadDiscourse m, MonadTrans t) => MonadDiscourse (t m) where
    getLatest = lift getLatest
