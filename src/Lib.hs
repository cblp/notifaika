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

module Lib where

-- component
import Cache
import Config
import Discourse
import EventSource
import Gitter
import Gitter.Monad
import Types
-- global
import            Control.Monad.Catch
import            Control.Monad.Reader
import            Data.List as List
import qualified  Data.Set  as Set
import            Data.Text ( Text )
import qualified  Data.Text as Text

-- | Takes (cache, current topics) and returns (new cache, new topics)
detectNewTopics :: (Maybe [Eid], [Topic]) -> ([Eid], [Topic])
detectNewTopics (Nothing, current) =
    (fmap topic_eid current, [])
detectNewTopics (Just olds, current) =
    let oldsSet = Set.fromList olds
        isNew topic = Set.notMember (topic_eid topic) oldsSet
        news = filter isNew current
    in  (olds `union` fmap topic_eid news, news)

makeMessage :: EventSource -> Topic -> Text
makeMessage = error "makeMessage"

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
        currentTopics <- getTopics source
        cachedTopics <- Cache.load source
        let (cachedTopics', newTopics) =
                detectNewTopics (cachedTopics, currentTopics)
        forM_ newTopics $ \topic -> do
            let message = makeMessage source topic
            -- TODO move withRoom above forM_
            Gitter.withRoom room (sendChatMessage message)
        when (cachedTopics /= Just cachedTopics') $
            Cache.save source cachedTopics'

showText :: Show a => a -> Text
showText = Text.pack . show
