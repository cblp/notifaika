{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}

module Cache where

-- component
import Discourse
-- general
import Control.Monad.Logger
import Control.Monad.State
import Control.Monad.Writer

class MonadCache a m where
    loadDef :: a -> m a
    save :: a -> m ()

newtype FileCacheT m a = FileCacheT { runFileCacheT :: FilePath -> m a }
    deriving Functor

instance MonadTrans FileCacheT where
    lift = error "unimplemented lift@FileCacheT"

instance Functor m => Applicative (FileCacheT m) where
    pure = undefined
    (<*>) = undefined

instance Functor m => Monad (FileCacheT m) where
    (>>=) = undefined

instance Functor m => MonadCache a (FileCacheT m) where
    loadDef = undefined
    save = undefined

instance Functor m => MonadLogger (FileCacheT m) where
    monadLoggerLog = undefined

instance Monad m => MonadCache s (StateT s m) where
    loadDef _ = get
    save = put

instance (Monad m, MonadCache a m, Monoid w) => MonadCache a (WriterT w m) where
    loadDef = lift . loadDef
    save = lift . save

instance (Monad m, MonadDiscourse m) => MonadDiscourse (FileCacheT m) where
    getLatest = lift getLatest
