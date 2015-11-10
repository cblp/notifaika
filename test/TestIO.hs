{-# LANGUAGE  FlexibleInstances
            , GeneralizedNewtypeDeriving
            , MultiParamTypeClasses
            , NamedFieldPuns
  #-}

module TestIO (Effect(..), TestIO, TestIOResult(..), execTestIO) where

-- package
import Cache
import Config
import Discourse
import EventSource
import Gitter.Monad
import Gitter.Types
-- general
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
    getTopics (Discourse url) = TestIO $ do
        tell [EventsGet]
        let dataFileName = url  & String.replace "/" "."
                                & String.replace ":" "."
                                & String.replace "..." "."
            dataFilePath = "test/data/discourse" </> dataFileName </> "latest.json"
        dataFileExists <- liftIO (doesFileExist dataFilePath)
        if dataFileExists then do
            Latest{latest_topic_list=TopicList{topicList_topics}}
                <- liftIO (decodeFile dataFilePath)
            return topicList_topics
        else
            return []

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
