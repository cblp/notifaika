{-# LANGUAGE UndecidableInstances #-}

module Cache where

-- component
import Discourse
import Gitter.Monad
-- global
import Control.Error
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Data.Aeson
import Data.ByteString.Lazy as ByteString

class MonadCache a m where
    loadDef :: FromJSON a => a -> m a
    save :: ToJSON a => a -> m ()

newtype FileCacheT m a = FileCacheT (ReaderT FilePath m a)
    deriving (Applicative, Functor, Monad, MonadThrow, MonadTrans)

runFileCacheT :: FilePath -> FileCacheT m a -> m a
runFileCacheT filePath (FileCacheT readerAction) =
    runReaderT readerAction filePath

instance MonadIO io => MonadCache a (FileCacheT io) where
    loadDef def = FileCacheT $ do
        filePath <- ask
        mvalue <- liftIO . runMaybeT $ do
            contents <- hushT . tryIO $ ByteString.readFile filePath
            hoistMaybe (decode contents)
        return (fromMaybe def mvalue)
    save value = FileCacheT $ do
        filePath <- ask
        liftIO . ByteString.writeFile filePath $ encode value

instance Monad m => MonadCache s (StateT s m) where
    loadDef _ = get
    save = put

instance (Monad m, Monoid w) => MonadCache s (RWST r w s m) where
    loadDef _ = get
    save = put

instance (Monad m, MonadCache a m, Monoid w) => MonadCache a (WriterT w m) where
    loadDef = lift . loadDef
    save = lift . save

instance (Monad m, MonadCache a m) => MonadCache a (ReaderT r m) where
    loadDef = lift . loadDef
    save = lift . save

deriving instance (Monad m, MonadDiscourse m) => MonadDiscourse (FileCacheT m)

instance MonadReader r m => MonadReader r (FileCacheT m) where
    ask = lift ask
    local f (FileCacheT myReaderAction) = FileCacheT $ do
        file <- ask
        let nestedReaderAction = runReaderT myReaderAction file
        lift (local f nestedReaderAction)
    reader = lift . reader

deriving instance MonadGitter m => MonadGitter (FileCacheT m)

deriving instance MonadIO m => MonadIO (FileCacheT m)
