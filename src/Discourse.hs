module Discourse where

-- component
import EventSource
-- global
import Prelude
import Data.Aeson
import Data.Aeson.TH
import Data.String.X
import Data.Text

data User = User { user_id :: Int, user_username :: Text }

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "user_"} ''User

data TopicList = TopicList { topicList_topics :: [Topic] }

deriveJSON  defaultOptions{fieldLabelModifier = dropPrefix "topicList_"}
            ''TopicList

data Latest = Latest { latest_topic_list :: TopicList, latest_users :: [User] }

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "latest_"} ''Latest

resultToEither :: Result a -> Either String a
resultToEither (Success s)  = Right s
resultToEither (Error e)    = Left e
