module Discourse where

import Prelude hiding ( lookup )
import Control.Error
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import Data.Monoid
import Data.Ord
import Data.String.X
import Data.Time
import Network.Wreq

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

data Discourse = Discourse { discourse_baseUrl :: String }

newtype DiscourseT m a = DiscourseT (ReaderT Discourse m a)
    deriving (Applicative, Functor, Monad, MonadIO, MonadThrow)

runDiscourseT :: Discourse -> DiscourseT m a -> m a
runDiscourseT discourse (DiscourseT readerAction) =
    runReaderT readerAction discourse

instance MonadDiscourse (DiscourseT IO) where
    getLatest = DiscourseT $ do
        Discourse{..} <- ask
        response <- liftIO $ get (discourse_baseUrl <> "/latest.json")
        jsonResponse <- asJSON response
        let jsonBody = fromMaybe  (error "Can't decode Discourse response")
                                  (jsonResponse ^? responseBody)
        case decodeLatestResponse jsonBody of
            Left e -> fail e
            Right topics -> return topics

instance (Monad m, MonadDiscourse m) => MonadDiscourse (LoggingT m) where
    getLatest = lift getLatest

instance (Monad m, MonadDiscourse m) => MonadDiscourse (ReaderT r m) where
    getLatest = lift getLatest
