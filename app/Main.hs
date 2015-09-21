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

main :: IO ()
main =  run repostUpdates
            "/etc/discourse-to-gitter.cfg"
            "/var/cache/discourse-to-gitter/latest"
  where
    run action configFile cacheFile = do
        config <- loadConfig configFile
        let gitter = Gitter { gitter_baseUrl = config ^. config_gitterBaseUrl }
        runFileCacheT
            (runGitterT gitter $ runReaderT (runStderrLoggingT action) config)
            cacheFile

loadConfig :: FilePath -> IO Config
loadConfig filePath = do
    contents <- ByteString.readFile filePath
    either fail return $ eitherDecode contents
