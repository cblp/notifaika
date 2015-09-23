module Config where

-- component
import Gitter.Types
-- global
import Control.Lens
import Data.Aeson.TH
import Data.String.X

data Config = Config  { _config_cacheFile :: FilePath
                      , _config_discourseBaseUrl :: String
                      , _config_gitterBaseUrl :: String
                      , _config_gitterRoom :: Room
                      }
makeLenses ''Config

deriveFromJSON  defaultOptions { fieldLabelModifier = dropPrefix "_config_" }
                ''Config
