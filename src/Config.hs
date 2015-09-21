module Config where

-- component
import Gitter.Types
-- global
import Control.Lens
import Data.Aeson.TH

data Config = Config { _config_gitterBaseUrl :: String, _config_room :: Room }
makeLenses ''Config

deriveFromJSON defaultOptions ''Config
