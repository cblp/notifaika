{-# LANGUAGE NamedFieldPuns #-}

module Discourse where

-- component
import Types
-- global
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Monoid
import Data.String.X
import Data.Text    as Text
import Network.Wreq as Wreq

data User = User { user_id :: Int, user_username :: Text }

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "user_"} ''User

data Poster = Poster { poster_user_id :: Int }
    deriving (Eq, Show)

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "poster_"} ''Poster

data Topic = Topic  { topic_fancy_title :: Text
                    , topic_id :: Integer
                    , topic_posters :: [Poster]
                    , topic_slug :: Text
                    }
    deriving (Eq, Show)

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "topic_"} ''Topic

topic_eid :: Topic -> Eid
topic_eid = Eid . Text.pack . show . topic_id

data TopicList = TopicList { topicList_topics :: [Topic] }

deriveJSON  defaultOptions{fieldLabelModifier = dropPrefix "topicList_"}
            ''TopicList

data Latest = Latest { latest_topic_list :: TopicList, latest_users :: [User] }

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "latest_"} ''Latest

resultToEither :: Result a -> Either String a
resultToEither (Success s)  = Right s
resultToEither (Error e)    = Left e

getDiscourseTopics :: (MonadIO m, MonadThrow m) => Url -> m [Topic]
getDiscourseTopics baseUrl = do
    let url = baseUrl <> "/latest.json"
    response <- liftIO $ Wreq.get url
    jsonResponse <- asJSON response
    let Latest{latest_topic_list=TopicList{topicList_topics}} =
            jsonResponse ^. responseBody
    return topicList_topics
