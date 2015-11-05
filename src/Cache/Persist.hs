{-# LANGUAGE QuasiQuotes, UndecidableInstances #-}

module Cache.Persist where

-- component
import Cache
import EventSource
import Gitter.Monad
-- global
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Set   as Set
import Data.Text  ( Text )
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Source
        repr    String

        UniqueSource repr

    Event
        source  SourceId
        eid     Eid

        UniqueItem source eid
|]

type DatabaseConnectionString = Text

newtype PersistCacheT m a = PersistCacheT (ReaderT DatabaseConnectionString m a)
    deriving (Applicative, Functor, Monad, MonadThrow, MonadTrans)

runPersistCacheT  :: (MonadBaseControl IO m, MonadIO m)
                  => DatabaseConnectionString -> PersistCacheT m a -> m a
runPersistCacheT database (PersistCacheT readerAction) = do
    runSqlite database $
        runMigration migrateAll
    runReaderT readerAction database

instance (MonadBaseControl IO io, MonadIO io) => MonadCache (PersistCacheT io) where
    load source = PersistCacheT $ do
        database <- ask
        runSqlite database $ do
            mSourceEntity <- selectFirst [SourceRepr ==. show source] []
            -- TODO MaybeT
            case mSourceEntity of
                Nothing -> return Nothing
                Just Entity{entityKey=sourceId} -> do
                    events <- selectList [EventSource ==. sourceId] []
                    return $ Just [eventEid event | Entity{entityVal=event} <- events]
    save source eids = PersistCacheT $ do
        database <- ask
        runSqlite database $ do
            Entity{entityKey=sourceId} <-
                upsert Source{sourceRepr = show source} []
            oldEntities <- selectList [EventSource ==. sourceId] []
            let oldEids =
                    Set.fromList  [ eventEid event
                                  | Entity{entityVal=event} <- oldEntities
                                  ]
            insertMany_ [ Event{eventSource=sourceId, eventEid=eid}
                        | eid <- eids
                        , eid `Set.notMember` oldEids
                        ]

instance MonadReader r m => MonadReader r (PersistCacheT m) where
    ask = lift ask
    local f (PersistCacheT myReaderAction) = PersistCacheT $ do
        file <- ask
        let nestedReaderAction = runReaderT myReaderAction file
        lift (local f nestedReaderAction)
    reader = lift . reader

deriving instance MonadGitter m => MonadGitter (PersistCacheT m)

deriving instance MonadIO m => MonadIO (PersistCacheT m)

deriving instance (Monad m, MonadEventSource m) =>
    MonadEventSource (PersistCacheT m)
