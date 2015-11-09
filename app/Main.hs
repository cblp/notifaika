module Main where

-- component
import Cache.Persist
import Config
import EventSource
import Gitter
import Gitter.Types
import Lib
-- general
import Control.Monad.Reader
import Data.String

main :: IO ()
main = do
    let config_cacheFile = "cache.sqlite"
        config_sources = [Discourse "http://forum.ruhaskell.org"]
        config_gitter = Gitter
            { gitter_baseUrl = "https://api.gitter.im/v1"
            , gitter_room = ONETOONE "cblp"
            , gitter_tokenFile = "../../felix/config/Gitter/HaskellCurry-token"
            }
        config = Config{..}
    runPersistCacheT (fromString config_cacheFile) .
        runGitterT config_gitter $
            runReaderT repostUpdates config
