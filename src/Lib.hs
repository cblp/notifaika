{-# LANGUAGE ConstraintKinds #-}

module Lib where

-- component
import Cache
import Config
import Discourse
import RSS
import Gitter
import Gitter.Monad
-- global
import            Control.Monad.Reader
import            Data.IntMap ( (!) )
import qualified  Data.IntMap as IntMap
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
    Latest{latest_topic_list = TopicList{topicList_topics = latestTopics}, ..}
        <- Discourse.getLatest
    let users = IntMap.fromList
            [(user_id, user_username) | User{..} <- latest_users]
    cachedTopics <- Cache.loadDef []
    let newTopics = detectNewTopics cachedTopics latestTopics
    Config{..} <- ask
    let room = gitter_room config_gitter
    forM_ newTopics $ \Topic{..} -> do
        let link = mconcat  [ Text.pack config_discourseBaseUrl
                            , "/t/", topic_slug, "/", showText topic_id ]
            Poster{..} = head topic_posters
            message = mconcat
                [ users ! poster_user_id, " опубликовал на форуме тему «"
                  , topic_fancy_title, "»\n"
                , link
                ]
        Gitter.withRoom room (sendChatMessage message)
    Cache.save latestTopics

-- | Load new RSS and announce them.
repostRSS :: ( MonadGitter m
             , MonadRss m
             , MonadReader Config m
             , MonadCache RSS.RSSCache m
             , MonadIO m) => m ()
repostRSS = do
    feeds <- RSS.loadFeeds
    cache <- Cache.loadDef RSS.emptyCache
    let (newCache, newItems) =
          RSS.updateFeeds (RSS.initializeFeeds cache feeds) feeds
    Config{..} <- ask
    let room = gitter_room config_gitter
    forM_ newItems $ \Item{..} -> do
        let message = mconcat
                [ "Новый пост «", item_title
                , "» в ленте «", item_channel, "»\n"
                , item_link
                ]
        Gitter.withRoom room (sendChatMessage message)
    Cache.save newCache

showText :: Show a => a -> Text
showText = Text.pack . show
