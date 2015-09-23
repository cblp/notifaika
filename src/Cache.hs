{-# LANGUAGE UndecidableInstances #-}

module Cache where

-- component
import Discourse
import Gitter.Monad
-- global
import Control.Error
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.X
import Control.Monad.Writer
import Data.Aeson
import Data.ByteString.Lazy as ByteString

class MonadCache a m where
    loadDef :: FromJSON a => a -> m a
    save :: ToJSON a => a -> m ()

newtype FileCacheT m a = FileCacheT (ReaderT FilePath m a)
    deriving (Applicative, Functor, Monad, MonadLogger)

runFileCacheT :: FilePath -> FileCacheT m a -> m a
runFileCacheT filePath (FileCacheT readerAction) =
    runReaderT readerAction filePath

instance MonadTrans FileCacheT where
    lift = FileCacheT . lift

instance MonadIO io => MonadCache a (FileCacheT io) where
    loadDef def = FileCacheT $ do
        file <- ask
        mvalue <- liftIO . runMaybeT $ do
            contents <- hushT . tryIO $ ByteString.readFile file
            hoistMaybe (decode contents)
        return (fromMaybe def mvalue)
    save = error "unimplemented save@FileCacheT"

instance Monad m => MonadCache s (StateT s m) where
    loadDef _ = get
    save = put

instance (Monad m, MonadCache a m, Monoid w) => MonadCache a (WriterT w m) where
    loadDef = lift . loadDef
    save = lift . save

instance (Monad m, Monoid w) => MonadCache s (RWST r w s m) where
    loadDef _ = get
    save = put

-- TODO derive
instance (Monad m, MonadDiscourse m) => MonadDiscourse (FileCacheT m) where
    getLatest = lift getLatest

instance MonadReader r m => MonadReader r (FileCacheT m) where
    ask = lift ask
    local f (FileCacheT myReaderAction) = FileCacheT $ do
        file <- ask
        let nestedReaderAction = runReaderT myReaderAction file
        lift (local f nestedReaderAction)
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
