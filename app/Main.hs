module Main where

-- component
import Cache
import Config
import Discourse
import Gitter
import Lib
-- general
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
        config@Config{..} <- loadConfig configFile
        let discourse = Discourse
                { discourse_baseUrl = config_discourseBaseUrl }
            gitter = config_gitter
        runDiscourseT discourse .
            runFileCacheT config_cacheFile $
                runGitterT gitter (runReaderT action config)

loadConfig :: FilePath -> IO Config
loadConfig filePath = do
    contents <- ByteString.readFile filePath
    either decodeError return $ eitherDecode contents
  where
    decodeError err = error ("Cannot decode " <> filePath <> ": " <> err)
