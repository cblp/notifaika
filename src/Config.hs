module Config where

-- component
import EventSource
import Gitter.Types
-- global
import Data.Aeson.TH
import Data.String.X

data Config = Config  { config_cacheFile :: FilePath
                      , config_sources :: [EventSource]
                      , config_gitter :: Gitter
                      }

deriveFromJSON  defaultOptions { fieldLabelModifier = dropPrefix "config_" }
                ''Config
