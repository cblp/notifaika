module Gitter.Types where

import Data.Aeson.TH
import Data.String.X
import Data.Text

type ResourcePath = [Text]
type UserName = Text
type RepoName = Text
type RoomId = Text
type RoomUri = Text

data Room = ONETOONE UserName | REPO UserName RepoName
    deriving Show

deriveJSON
    defaultOptions  { sumEncoding = TaggedObject  { tagFieldName = "type"
                                                  , contentsFieldName = "uri"
                                                  }
                    }
    ''Room

data Gitter = Gitter  { gitter_baseUrl :: String
                      , gitter_room :: Room
                      , gitter_tokenFile :: FilePath
                      }

deriveJSON defaultOptions { fieldLabelModifier = dropPrefix "gitter_" } ''Gitter
