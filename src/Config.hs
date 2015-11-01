module Config where

-- component
import Gitter.Types
import RSS.Types
-- global
import Data.Aeson.TH
import Data.String.X

data Config = Config  { config_cacheFile :: FilePath
                      , config_discourseBaseUrl :: String
                      , config_gitter :: Gitter
                      , config_rss    :: RSS  -- ^ configuration for RSS module.
                      , config_rssCache :: FilePath -- ^ RSS cache file.
                      }

deriveFromJSON  defaultOptions { fieldLabelModifier = dropPrefix "config_" }
                ''Config
