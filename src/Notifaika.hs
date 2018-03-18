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
    ( Config (..)
    , EventSource (..)
    , Gitter (..)
    , Room (..)
    , Target (..)
    , runNotifaika
    ) where

import Control.Monad.Reader (runReaderT)
import Data.String (fromString)
import Gitter (Gitter (..))
import Gitter.Types (Room (..))

import qualified Notifaika.Cache.Sqlite as Cache
import           Notifaika.Config (Config (..), Target (..))
import           Notifaika.Core (repostUpdates)
import           Notifaika.EventSource (EventSource (..))

runNotifaika :: Config -> IO ()
runNotifaika config@Config{configCacheFile} =
    Cache.runPersistCacheT (fromString configCacheFile) $
        runReaderT repostUpdates config
