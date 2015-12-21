{-
    Notifaika reposts notifications
    from different feeds to Gitter chats.
    Copyright (C) 2015 Yuriy Syrovetskiy

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Network.Gitter.Types where

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
