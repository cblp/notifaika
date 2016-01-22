{-
    Notifaika reposts notifications
    from different feeds to Gitter chats.
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

module Notifaika.Cache.Sqlite
  ( DatabaseConnectionString(..)
  , save
  , load
  , migrateAll
  , SourceId
  , EventId
  ) where

import Notifaika.Types ( Eid )
import Notifaika.EventSource

import Control.Monad.Classes
import Control.Monad.IO.Class
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

newtype DatabaseConnectionString = DatabaseConnectionString { getDatabaseConnectionString :: Text }

load :: (MonadIO m, MonadReader DatabaseConnectionString m) => EventSource -> m (Maybe [Eid])
load source = do
    database <- getDatabaseConnectionString <$> ask
    liftIO $ runSqlite database $ do
        mSourceEntity <- selectFirst [SourceRepr ==. show source] []
        -- TODO MaybeT
        case mSourceEntity of
            Nothing -> return Nothing
            Just Entity{entityKey=sourceId} -> do
                events <- selectList [EventSource ==. sourceId] []
                return $ Just [eventEid event | Entity{entityVal=event} <- events]

save :: (MonadIO m, MonadReader DatabaseConnectionString m) => EventSource -> [Eid] -> m ()
save source eids = do
    database <- getDatabaseConnectionString <$> ask
    liftIO $ runSqlite database $ do
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
