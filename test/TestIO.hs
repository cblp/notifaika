{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestIO (TestIO, Effect(..), execTestIO) where

-- package
import Discourse
-- general
import Control.Monad.Logger
import Control.Monad.Writer
import Data.Aeson
import Data.ByteString.Lazy as ByteString

data Effect = DiscourseGetLatestJson
    deriving (Eq, Show)

newtype TestIO a = TestIO { runTestIO :: WriterT [Effect] (LoggingT IO) a }
    deriving (Applicative, Functor, Monad, MonadLogger)

instance MonadDiscourse TestIO where
    getLatest = TestIO $ do
        tell [DiscourseGetLatestJson]
        lift . lift $
            either error id . decodeLatestResponse
                <$> decodeFile "test/data/discourse/latest.json"

decodeFile :: FromJSON a => FilePath -> IO a
decodeFile filepath = do
    bytes <- ByteString.readFile filepath
    let decodeResult = eitherDecode bytes
    case decodeResult of
        Left decodeError ->
            error ("Cannot decode file \"" <> filepath <> "\": " <> decodeError)
        Right value ->
            return value

execTestIO :: TestIO () -> IO [Effect]
execTestIO = runStderrLoggingT . execWriterT . runTestIO
