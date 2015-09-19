{-# LANGUAGE  FlexibleInstances
            , GeneralizedNewtypeDeriving
            , MultiParamTypeClasses
  #-}

module TestIO (Effect(..), TestIO, TestIOResult(..), execTestIO) where

-- package
import Cache
import Discourse
-- general
import Control.Monad.Logger
import Control.Monad.State
import Control.Monad.Writer
import Data.Aeson
import Data.ByteString.Lazy as ByteString

data Effect = CacheRead | CacheWrite | DiscourseGet String | GitterPost
    deriving (Eq, Show)

newtype TestIO a = TestIO (WriterT [Effect] (StateT [Post] (LoggingT IO)) a)
    deriving (Applicative, Functor, Monad, MonadLogger)

instance MonadCache [Post] TestIO where
    loadDef def = TestIO $ do
        tell [CacheRead]
        loadDef def
    save val = TestIO $ do
        tell [CacheWrite]
        save val

instance MonadDiscourse TestIO where
    getLatest = TestIO $ do
        tell [DiscourseGet "/latest.json"]
        liftIO $ do
            jsonContent <- decodeFile "test/data/discourse/latest.json"
            let latestPosts = either error id (decodeLatestResponse jsonContent)
            return latestPosts

decodeFile :: FromJSON a => FilePath -> IO a
decodeFile filepath = do
    bytes <- ByteString.readFile filepath
    let decodeResult = eitherDecode bytes
    case decodeResult of
        Left decodeError ->
            error ("Cannot decode file \"" <> filepath <> "\": " <> decodeError)
        Right value ->
            return value

data TestIOResult = TestIOResult  { testIOResult_effects  :: [Effect]
                                  , testIOResult_cache    :: [Post]
                                  }

execTestIO :: TestIO () -> IO TestIOResult
execTestIO testAction = do
    let cache = []
        TestIO writerAction = testAction
        stateAction = execWriterT writerAction
        loggingAction = runStateT stateAction cache
        ioAction = runStderrLoggingT loggingAction
    (testIOResult_effects, testIOResult_cache) <- ioAction
    return TestIOResult{..}
