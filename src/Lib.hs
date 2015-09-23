{-# LANGUAGE ConstraintKinds #-}

module Lib where

-- component
import Cache
import Config
import Discourse
import Gitter
import Gitter.Monad
-- global
import            Control.Monad.Reader
import            Data.Monoid
import            Data.Text ( Text )
import qualified  Data.Text as Text

detectNewTopics :: [Topic] -> [Topic] -> [Topic]
detectNewTopics []   =
    return . maximum
detectNewTopics olds =
    filter $ \topic ->
        any (\old -> topic_id old /= topic_id topic && old <= topic) olds

type MonadRepost m =  ( MonadCache [Topic] m
                      , MonadDiscourse m
                      , MonadGitter m
                      , MonadReader Config m
                      )

repostUpdates :: MonadRepost m => m ()
repostUpdates = do
    latestTopics <- Discourse.getLatest
    cachedTopics <- loadDef []
    let newTopics = detectNewTopics cachedTopics latestTopics
    room <- asks (gitter_room . config_gitter)
    let message = "new topics: " <> showText newTopics
    withRoom room (sendChatMessage message)
    save latestTopics

showText :: Show a => a -> Text
showText = Text.pack . show
