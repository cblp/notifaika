module Control.Monad.Trans.X where

import Control.Monad.Trans

lift2 :: (Monad m, MonadTrans t) => (a -> b -> m c) -> a -> b -> t m c
lift2 f a b = lift (f a b)
