module Config where

-- component
import EventSource
import Gitter.Types

data Config = Config  { config_cacheFile :: FilePath
                      , config_sources :: [EventSource]
                      , config_gitter :: Gitter
                      }
