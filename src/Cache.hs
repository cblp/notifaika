{-# LANGUAGE DeriveFunctor, UndecidableInstances #-}

module Cache where

-- component
import Discourse
import Gitter.Monad
-- global
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.X
import Control.Monad.Writer

class MonadCache a m where
    loadDef :: a -> m a
    save :: a -> m ()

newtype FileCacheT m a = FileCacheT { runFileCacheT :: FilePath -> m a }
    deriving Functor

instance MonadTrans FileCacheT where
    lift = error "unimplemented lift@FileCacheT"

instance Functor m => Applicative (FileCacheT m) where
    pure = error "unimplemented pure@FileCacheT"
    (<*>) = error "unimplemented (<*>)@FileCacheT"

instance Functor m => Monad (FileCacheT m) where
    (>>=) = error "unimplemented (>>=)@FileCacheT"

instance Functor m => MonadCache a (FileCacheT m) where
    loadDef = error "unimplemented loadDef@FileCacheT"
    save = error "unimplemented save@FileCacheT"

instance Functor m => MonadLogger (FileCacheT m) where
    monadLoggerLog = error "unimplemented monadLoggerLog@FileCacheT"

instance Monad m => MonadCache s (StateT s m) where
    loadDef _ = get
    save = put

instance (Monad m, MonadCache a m, Monoid w) => MonadCache a (WriterT w m) where
    loadDef = lift . loadDef
    save = lift . save

instance (Monad m, Monoid w) => MonadCache s (RWST r w s m) where
    loadDef _ = get
    save = put

instance (Monad m, MonadDiscourse m) => MonadDiscourse (FileCacheT m) where
    getLatest = lift getLatest

instance MonadReader r m => MonadReader r (FileCacheT m) where
    ask = lift ask
    local f (FileCacheT g) = FileCacheT (local f . g)
    reader = lift . reader

instance MonadGitter m => MonadGitter (FileCacheT m) where
    runGitterAction = lift2 runGitterAction

instance MonadIO m => MonadIO (FileCacheT m) where
    liftIO = lift . liftIO

instance (Monad m, MonadCache a m) => MonadCache a (LoggingT m) where
    loadDef = lift . loadDef
    save = lift . save

instance (Monad m, MonadCache a m) => MonadCache a (ReaderT r m) where
    loadDef = lift . loadDef
    save = lift . save
