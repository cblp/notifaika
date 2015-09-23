module Gitter.Types where

import Data.Aeson.TH
import Data.Text

type ResourcePath = [Text]
type UserName = Text
type RepoName = Text
type RoomId = Text
type RoomUri = Text

data Room = RoomOneToOne UserName | RoomRepo UserName RepoName

deriveJSON defaultOptions ''Room
