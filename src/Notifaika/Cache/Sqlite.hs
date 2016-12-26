{-
    Notifaika reposts notifications
    from different feeds to Gitter chats.
    Copyright (C) 2015-2016 Yuriy Syrovetskiy

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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Notifaika.Cache.Sqlite where

import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, local, reader,
                                       runReaderT)
import           Control.Monad.Trans (MonadTrans, lift)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Database.Persist (Entity(Entity), entityKey, entityVal,
                                   insertMany_, selectFirst, selectList, upsert,
                                   (==.))
import           Database.Persist.Sqlite (runMigration, runSqlite)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import           Network.Gitter.Monad (MonadGitter)

import           Notifaika.Cache (MonadCache, load, save)
import           Notifaika.EventSource (MonadEventSource)
import           Notifaika.Types (Eid)

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
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

runPersistCacheT :: DatabaseConnectionString -> PersistCacheT m a -> m a
runPersistCacheT database (PersistCacheT readerAction) =
    runReaderT readerAction database

instance
    (MonadBaseControl IO io, MonadIO io) =>
    MonadCache (PersistCacheT io) where

    load source = PersistCacheT $ do
        database <- ask
        runSqlite database $ do
            runMigration migrateAll
            runMaybeT $ do
                Entity{entityKey=sourceId} <-
                    MaybeT $ selectFirst [SourceRepr ==. show source] []
                lift $
                    map (eventEid . entityVal) <$>
                        selectList [EventSource ==. sourceId] []

    save source eids = PersistCacheT $ do
        database <- ask
        runSqlite database $ do
            runMigration migrateAll
            Entity{entityKey=sourceId} <-
                upsert Source{sourceRepr = show source} []
            oldEntities <- selectList [EventSource ==. sourceId] []
            let oldEids = Set.fromList $ eventEid . entityVal <$> oldEntities
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

deriving instance
    (Monad m, MonadEventSource m) =>
    MonadEventSource (PersistCacheT m)
