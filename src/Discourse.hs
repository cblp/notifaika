module Discourse where

import Prelude hiding ( lookup )
import Control.Error
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import Data.Ord
import Data.String.Extra
import Data.Time

data Topic = Topic { topic_id :: Integer, topic_created_at :: UTCTime }
    deriving (Eq, Show)

instance Ord Topic where
    compare = comparing topic_created_at

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "topic_"} ''Topic

-- | decode response to "/latest.json" request
decodeLatestResponse :: Value -> Either String [Topic]
decodeLatestResponse response = do
    topicList <- note "\"latest\" must have key \"topic_list\""
        (response ^? key "topic_list")
    topics <- note "\"latest.topic_list\" must have key \"topics\""
        (topicList ^? key "topics")
    resultToEither (fromJSON topics)

resultToEither :: Result a -> Either String a
resultToEither (Success s)  = Right s
resultToEither (Error e)    = Left e

class MonadDiscourse m where
    getLatest :: m [Topic]

instance MonadDiscourse IO where
    getLatest = error "not implemented getLatest@IO"

instance (Monad m, MonadDiscourse m) => MonadDiscourse (LoggingT m) where
    getLatest = lift getLatest

instance (Monad m, MonadDiscourse m) => MonadDiscourse (ReaderT r m) where
    getLatest = lift getLatest
