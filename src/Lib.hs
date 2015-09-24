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
import qualified  Data.Set as Set
import            Data.Text ( Text )
import qualified  Data.Text as Text

detectNewTopics :: [Topic] -> [Topic] -> [Topic]
detectNewTopics []   =
    return . maximum
detectNewTopics olds =
    let oldIds = Set.fromList (fmap topic_id olds)
    in  filter $ \topic@Topic{ topic_id = tid } ->
            Set.notMember tid oldIds
            && any (\old -> topic_id old /= tid && old <= topic) olds

type MonadRepost m =  ( MonadCache [Topic] m
                      , MonadDiscourse m
                      , MonadGitter m
                      , MonadReader Config m
                      )

repostUpdates :: MonadRepost m => m ()
repostUpdates = do
    latestTopics <- Discourse.getLatest
    cachedTopics <- Cache.loadDef []
    let newTopics = detectNewTopics cachedTopics latestTopics
    Config{..} <- ask
    let room = gitter_room config_gitter
    forM_ newTopics $ \Topic{..} -> do
        let link = mconcat  [ Text.pack config_discourseBaseUrl
                            , "/t/", topic_slug, "/", showText topic_id ]
            Poster{..} = head topic_posters
            message = mconcat
                [ showText poster_user_id, " опубликовал на форуме тему «"
                  , topic_fancy_title, "»\n"
                , link
                ]
        Gitter.withRoom room (sendChatMessage message)
    Cache.save latestTopics

showText :: Show a => a -> Text
showText = Text.pack . show
