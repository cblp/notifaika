{-
    Discourse-to-Gitter reposts notification
    from Discourse forums to Gitter chats.
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

module Main where

import Notifaika

main :: IO ()
main = runNotifaika Config
    { config_cacheFile = "cache.sqlite"
    , config_sources =
          [ Discourse "http://forum.ruhaskell.org"
          , RSS "http://bananasandlenses.net/feed.xml"
          ]
    , config_gitter = Gitter
          { gitter_baseUrl = "https://api.gitter.im/v1"
          , gitter_room = ONETOONE "cblp"
          , gitter_tokenFile = "../../felix/config/Gitter/HaskellCurry-token"
          }
    }
