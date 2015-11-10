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

{-# LANGUAGE ConstraintKinds, NamedFieldPuns #-}

module Notifaika where

import Notifaika.Cache  as Cache
import Notifaika.Config
import Notifaika.EventSource
import Notifaika.Gitter as Gitter
import Notifaika.Gitter.Monad
import Notifaika.Types

import            Control.Monad.Catch
import            Control.Monad.Reader
import            Data.List as List
import qualified  Data.Set  as Set

-- | Takes (cache, current topics) and returns (new cache, new topics)
detectNewEvents :: (Maybe [Eid], [Event]) -> ([Eid], [Event])
detectNewEvents (Nothing, current) =
    (fmap eventId current, [])
detectNewEvents (Just olds, current) =
    let oldsSet = Set.fromList olds
        isNew Event{eventId} = Set.notMember eventId oldsSet
        news = filter isNew current
    in  (olds `union` fmap eventId news, news)

type MonadRepost m =  ( MonadCache m
                      , MonadGitter m
                      , MonadEventSource m
                      , MonadIO m
                      , MonadReader Config m
                      , MonadThrow m
                      )

repostUpdates :: MonadRepost m => m ()
repostUpdates = do
    Config{config_gitter, config_sources} <- ask
    let room = gitter_room config_gitter
    forM_ config_sources $ \source -> do
        currentEvents <- getEvents source
        cachedEvents <- Cache.load source
        let (cachedEvents', newEvents) =
                detectNewEvents (cachedEvents, currentEvents)
        forM_ newEvents $ \Event{message} ->
            -- TODO move withRoom above forM_
            Gitter.withRoom room (sendChatMessage message)
        when (cachedEvents /= Just cachedEvents') $
            Cache.save source cachedEvents'
