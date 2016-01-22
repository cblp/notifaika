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

{-# LANGUAGE NamedFieldPuns #-}

module Notifaika
    ( Config(..)
    , EventSource(..)
    , Gitter(..)
    , Room(..)
    , runNotifaika
    ) where

import qualified  Notifaika.Cache.Sqlite  as Cache
import            Notifaika.Config
import            Notifaika.Core
import            Notifaika.EventSource

import Control.Monad.Classes.Run
import Data.String
import Network.Gitter as Gitter
import Network.Gitter.Types

runNotifaika :: Config -> IO ()
runNotifaika config@Config{config_cacheFile, config_gitter} =
    runGitterT config_gitter $
       runReader (Cache.DatabaseConnectionString $ fromString config_cacheFile) $
         runReader config repostUpdates
