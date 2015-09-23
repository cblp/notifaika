module Main where

-- component
import Cache
import Config
import Gitter
import Lib
-- general
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy as ByteString
import Data.Monoid
import System.Environment

usage :: String
usage = "\nUsage:\n  discourse-to-gitter CONFIG_FILE"

main :: IO ()
main = do
    args <- getArgs
    let configFile = case args of
            [cnf] -> cnf
            _ -> error usage
    run configFile repostUpdates
  where
    run configFile action = do
        config <- loadConfig configFile
        let gitter = Gitter { gitter_baseUrl = config ^. config_gitterBaseUrl }
            cacheFile = config ^. config_cacheFile
        runFileCacheT
            (runGitterT gitter $ runReaderT (runStderrLoggingT action) config)
            cacheFile

loadConfig :: FilePath -> IO Config
loadConfig filePath = do
    contents <- ByteString.readFile filePath
    either decodeError return $ eitherDecode contents
  where
    decodeError err = error ("Cannot decode " <> filePath <> ": " <> err)
