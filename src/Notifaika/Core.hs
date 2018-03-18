{-
    Notifaika reposts notifications
    from different feeds to Gitter chats.
    Copyright (C) 2015-2016 Yuriy Syrovetskiy

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

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Notifaika.Core (repostUpdates) where

import           Control.Monad (when)
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader, ask)
import           Data.Foldable (for_)
import qualified Data.List as List
import qualified Data.Set as Set
import           Data.Text (Text)
import           Gitter (Gitter (..), gitterRoom, runGitterT, sendChatMessage,
                         withRoom)

import           Notifaika.Cache (MonadCache)
import qualified Notifaika.Cache as Cache
import           Notifaika.Config (Config (..), Target (..))
import           Notifaika.EventSource (MonadEventSource, getEvents)
import           Notifaika.Types (Eid, Event (Event), eventId, message)

-- | Takes (cache, current topics) and returns (new cache, new topics)
detectNewEvents :: (Maybe [Eid], [Event]) -> ([Eid], [Event])
detectNewEvents (Nothing, current) = (fmap eventId current, [])
detectNewEvents (Just olds, current) =
    let oldsSet = Set.fromList olds
        isNew Event{eventId} = Set.notMember eventId oldsSet
        news = filter isNew current
    in  (olds `List.union` fmap eventId news, news)

type MonadRepost m =  ( MonadCache m
                      , MonadEventSource m
                      , MonadIO m
                      , MonadReader Config m
                      , MonadThrow m
                      )

repostUpdates :: MonadRepost m => m ()
repostUpdates = do
    Config{configTarget, configSources} <- ask
    let post = case configTarget of
            TargetGitter cfg -> postGitter cfg
    for_ configSources $ \source -> do
        currentEvents <- getEvents source
        cachedEvents <- Cache.load source
        let (cachedEvents', newEvents) =
                detectNewEvents (cachedEvents, currentEvents)
        for_ newEvents $ \Event{message} -> post message
        when (cachedEvents /= Just cachedEvents') $
            Cache.save source cachedEvents'

postGitter :: (MonadThrow m, MonadIO m) => Gitter -> Text -> m ()
postGitter cfg@Gitter{gitterRoom} message =
    runGitterT cfg $ withRoom gitterRoom $ sendChatMessage message
