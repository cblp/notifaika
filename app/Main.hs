module Main where

-- component
import Cache.Persist
import Config
import Gitter
import Lib
-- general
import Control.Monad.Reader
import Data.Aeson.X
import Data.String
import System.Environment

usage :: String
usage = "\nUsage:\n  discourse-to-gitter CONFIG_FILE"

main :: IO ()
main = do
    args <- getArgs
    let configFile = case args of
            [cnf] -> cnf
            _ -> error usage
    config@Config{..} <- decodeFile configFile
    runPersistCacheT (fromString config_cacheFile) .
        runGitterT config_gitter $
            runReaderT repostUpdates config
