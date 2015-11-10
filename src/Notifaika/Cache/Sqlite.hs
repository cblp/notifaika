{-
    Discourse-to-Gitter reposts notification
    from Discourse forums to Gitter chats.
    Copyright (C) 2015 Yuriy Syrovetskiy

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE QuasiQuotes, UndecidableInstances #-}

module Notifaika.Cache.Sqlite where

import Notifaika.Cache
import Notifaika.EventSource
import Notifaika.Gitter.Monad
import Notifaika.Types ( Eid )

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
