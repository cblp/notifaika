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

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Notifaika

main :: IO ()
main = runNotifaika Config
    { configCacheFile = "cache.sqlite"
    , configSources =
          [ RSS "http://bananasandlenses.net/feed.xml"
          , RSS "https://www.reddit.com/r/ruhaskell/.rss"
          ]
    , configTarget = TargetGitter Gitter
          { gitterBaseUrl = "https://api.gitter.im/v1"
          --, gitterRoom = ONETOONE "cblp"
          , gitterRoom = REPO "ruHaskell" "forall"
          , gitterTokenFile = "../../felix/config/Gitter/HaskellCurry-token"
          }
    }
