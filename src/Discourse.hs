module Discourse where

import Prelude hiding ( lookup )
import Control.Error
import Control.Lens
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import Data.Ord
import Data.String.Extra
import Data.Time

data Post = Post { post_id :: Int, post_created_at :: UTCTime }
    deriving (Eq, Show)

instance Ord Post where
    compare = comparing post_created_at

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "post_"} ''Post

-- | decode response to "/latest.json" request
decodeLatestResponse :: Value -> Either String [Post]
decodeLatestResponse response = do
    topicList <- note "\"latest\" must have key \"topic_list\""
        (response ^? key "topic_list")
    topics <- note "\"latest.topic_list\" must have key \"topic\""
        (topicList ^? key "topics")
    resultToEither (fromJSON topics)

resultToEither :: Result a -> Either String a
resultToEither (Success s)  = Right s
resultToEither (Error e)    = Left e

class MonadDiscourse m where
    getLatest :: m [Post]

instance MonadDiscourse IO where
    getLatest = error "not implemented getLatest@IO"

instance (Monad m, MonadDiscourse m, MonadTrans t) => MonadDiscourse (t m) where
    getLatest = lift getLatest
