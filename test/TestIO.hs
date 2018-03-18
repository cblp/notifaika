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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TestIO (Effect (..), TestIO, TestIOResult (..), execTestIO) where

import           Control.Monad (join)
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.RWS.Strict (MonadReader, RWST, execRWST, gets,
                                           modify', tell)
import           Data.Aeson.X (Value (Object), decodeFile)
import           Data.Function ((&))
import           Data.List (union)
import           Data.List.Extra (replace)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Semigroup ((<>))
import           Gitter.Monad (MonadGitter, runGitterAction)
import           Gitter.Types (Gitter (..), ResourcePath, Room (ONETOONE))
import           System.Directory (doesFileExist)
import           System.FilePath ((</>))

import Notifaika.Cache (MonadCache, load, save)
import Notifaika.Config (Config (..), Target (..))
import Notifaika.Discourse (extractEvents)
import Notifaika.EventSource (EventSource (Discourse, RSS), MonadEventSource,
                              getEvents)
import Notifaika.Types (Eid)

data Effect = CacheRead
            | CacheWrite
            | EventsGet
            | GitterAction ResourcePath
    deriving (Eq, Show)

type TestCache = Map EventSource (Maybe [Eid])

newtype TestIO a = TestIO (RWST Config [Effect] TestCache IO a)
    deriving
        (Applicative, Functor, Monad, MonadIO, MonadReader Config, MonadThrow)

instance MonadCache TestIO where
    load source = TestIO $ do
        tell [CacheRead]
        gets $ join . Map.lookup source

    save source eids = TestIO $ do
        tell [CacheWrite]
        modify' $ Map.insertWith munion source $ Just eids
      where
        munion Nothing    mys       = mys
        munion mxs        Nothing   = mxs
        munion (Just xs)  (Just ys) = Just (xs `union` ys)

instance MonadGitter TestIO where
    runGitterAction path body = TestIO $ do
        tell [GitterAction path]
        pure (mockGitter path body)
      where
        mockGitter :: ResourcePath -> Value -> Value
        mockGitter url req =
            let err = error
                    ("don't know how to mock " <> show url <> " " <> show req)
            in case url of
                ["rooms"] -> case req of
                    Object [("uri", "cblp")] -> Object [("id", "exampleroomid")]
                    _ -> err
                ["room", "exampleroomid", "chatMessages"] ->
                    case req of
                        "{\"text\":\"new topic!\"}" -> "{}"
                        _ -> err
                _ -> error ("don't know how to mock " <> show url)

instance MonadEventSource TestIO where
    getEvents (Discourse url) = TestIO $ do
        tell [EventsGet]
        let dataFileName =
                url & replace "/" "." & replace ":" "." & replace "..." "."
        let dataFilePath =
                "test/data/discourse" </> dataFileName </> "latest.json"
        dataFileExists <- liftIO $ doesFileExist dataFilePath
        if dataFileExists then do
            latest <- liftIO $ decodeFile dataFilePath
            pure $ extractEvents url latest
        else
            pure []
    getEvents (RSS _) = TestIO $ pure []

data TestIOResult = TestIOResult
    { testIOResult_effects :: [Effect]
    , testIOResult_cache :: TestCache
    }

execTestIO :: TestCache -> TestIO () -> IO TestIOResult
execTestIO initCache testAction = do
    let cacheFile = "test.sqlite"
        sources = Map.keys initCache
        config = Config
            { configCacheFile = cacheFile
            , configSources = sources
            , configTarget = TargetGitter Gitter
                  { gitterBaseUrl = "test://api.gitter.example.com/v1"
                  , gitterRoom = ONETOONE "cblp"
                  , gitterTokenFile = "/dev/null"
                  }
            }

    let TestIO rwsAction = testAction
        ioAction = execRWST rwsAction config initCache
    (testIOResult_cache, testIOResult_effects) <- ioAction
    pure TestIOResult{..}
