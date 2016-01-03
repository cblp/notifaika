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

{-# LANGUAGE ConstraintKinds, NamedFieldPuns #-}

module Notifaika.Core where

import Notifaika.Cache  as Cache
import Notifaika.Config
import Notifaika.EventSource
import Notifaika.Types

import            Data.List as List
import qualified  Data.Set  as Set
import            Network.Gitter as Gitter
import            Network.Gitter.Eff()

import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.Lift
import Data.Foldable (forM_)
import Control.Monad (when)

-- | Takes (cache, current topics) and returns (new cache, new topics)
detectNewEvents :: (Maybe [Eid], [Event]) -> ([Eid], [Event])
detectNewEvents (Nothing, current) =
    (fmap eventId current, [])
detectNewEvents (Just olds, current) =
    let oldsSet = Set.fromList olds
        isNew Event{eventId} = Set.notMember eventId oldsSet
        news = filter isNew current
    in  (olds `union` fmap eventId news, news)

repostUpdates :: ( SetMember Lift (Lift IO) r
                 , Member (Reader Config) r
                 , Member (Reader Gitter) r
                 , MonadCache (Eff r)
                 ) => Eff r ()
repostUpdates = do
    Config{config_gitter, config_sources} <- ask
    let room = gitter_room config_gitter
    forM_ config_sources $ \source -> do
        currentEvents <- getEvents source
        cachedEvents <- Cache.load source
        let (cachedEvents', newEvents) =
                detectNewEvents (cachedEvents, currentEvents)
        forM_ newEvents $ \Event{message} ->
            Gitter.withRoom room (sendChatMessage message)
        when (cachedEvents /= Just cachedEvents') $
            Cache.save source cachedEvents'
