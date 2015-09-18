{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestIO (TestIO, execTestIO) where

-- package
import Discourse
-- general
import Control.Monad.Logger
import Control.Monad.Writer
import Data.Aeson
import Data.ByteString.Lazy as ByteString

data WorldChange = MissileLaunch
    deriving (Eq, Show)

newtype TestIO a = TestIO { runTestIO :: WriterT [WorldChange] (LoggingT IO) a }
    deriving (Applicative, Functor, Monad, MonadLogger)

instance MonadDiscourse TestIO where
    getLatest =
        TestIO . lift . lift $ decodeFile "test/data/discourse/latest.json"

decodeFile :: FromJSON a => FilePath -> IO a
decodeFile filepath = do
    bytes <- ByteString.readFile filepath
    let decodeResult = eitherDecode bytes
    case decodeResult of
        Left decodeError ->
            error $ "Cannot decode file \"" <> filepath <> "\": " <> decodeError
        Right value ->
            return value

execTestIO :: TestIO () -> IO [WorldChange]
execTestIO = runStderrLoggingT . execWriterT . runTestIO
