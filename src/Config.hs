module Config where

-- component
import Gitter.Types
-- global
import Data.Aeson.TH
import Data.String.X

data Config = Config  { config_cacheFile :: FilePath
                      , config_discourseBaseUrl :: String
                      , config_gitter :: Gitter
                      }

deriveFromJSON  defaultOptions { fieldLabelModifier = dropPrefix "config_" }
                ''Config
