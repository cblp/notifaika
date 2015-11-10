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

{-# LANGUAGE  FlexibleInstances
            , GeneralizedNewtypeDeriving
            , MultiParamTypeClasses
            , NamedFieldPuns
  #-}

module TestIO (Effect(..), TestIO, TestIOResult(..), execTestIO) where

import Notifaika.Cache
import Notifaika.Config
import Notifaika.Discourse
import Notifaika.EventSource
import Notifaika.Gitter.Monad
import Notifaika.Gitter.Types
import Notifaika.Types

import            Control.Monad.Catch
import            Control.Monad.RWS
import            Data.Aeson.X
import            Data.Function
import            Data.List         as List
import            Data.Map          as Map
import qualified  Data.String.Utils as String
import            System.Directory
import            System.FilePath

data Effect = CacheRead
            | CacheWrite
            | EventsGet
            | GitterAction ResourcePath
    deriving (Eq, Show)

type TestCache = Map EventSource (Maybe [Eid])

newtype TestIO a = TestIO (RWST Config [Effect] TestCache IO a)
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config, MonadThrow)

instance MonadCache TestIO where
    load source = TestIO $ do
        tell [CacheRead]
        gets (join . Map.lookup source)
    save source eids = TestIO $ do
        tell [CacheWrite]
        modify (Map.insertWith munion source (Just eids))
      where
        munion Nothing    mys       = mys
        munion mxs        Nothing   = mxs
        munion (Just xs)  (Just ys) = Just (xs `List.union` ys)

instance MonadGitter TestIO where
    runGitterAction path body = TestIO $ do
        tell [GitterAction path]
        return (mockGitter path body)
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
        let dataFileName = url  & String.replace "/" "."
                                & String.replace ":" "."
                                & String.replace "..." "."
            dataFilePath = "test/data/discourse" </> dataFileName </> "latest.json"
        dataFileExists <- liftIO (doesFileExist dataFilePath)
        if dataFileExists then do
            latest <- liftIO (decodeFile dataFilePath)
            return (extractEvents url latest)
        else
            return []
    getEvents (RSS _) = TestIO $ return []

data TestIOResult = TestIOResult
    { testIOResult_effects  :: [Effect]
    , testIOResult_cache    :: TestCache
    }

execTestIO :: TestCache -> TestIO () -> IO TestIOResult
execTestIO initCache testAction = do
    let cacheFile = "test.sqlite"
        sources = Map.keys initCache
        config = Config
            { config_cacheFile = cacheFile
            , config_sources = sources
            , config_gitter = Gitter
                  { gitter_baseUrl = "test://api.gitter.example.com/v1"
                  , gitter_room = ONETOONE "cblp"
                  , gitter_tokenFile = "/dev/null"
                  }
            }

    let TestIO rwsAction = testAction
        ioAction = execRWST rwsAction config initCache
    (testIOResult_cache, testIOResult_effects) <- ioAction
    return TestIOResult{..}
