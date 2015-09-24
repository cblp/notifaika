module Discourse where

import Prelude
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.Monoid
import Data.Ord
import Data.String.X
import Data.Text
import Data.Time
import Network.Wreq

data Poster = Poster { poster_user_id :: Int }
    deriving (Eq, Show)

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "poster_"} ''Poster

data Topic = Topic  { topic_created_at :: UTCTime
                    , topic_fancy_title :: Text
                    , topic_id :: Integer
                    , topic_posters :: [Poster]
                    , topic_slug :: Text
                    }
    deriving (Eq, Show)

instance Ord Topic where
    compare = comparing topic_created_at

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "topic_"} ''Topic

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

class MonadDiscourse m where
    getLatest :: m Latest

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
        return (jsonResponse ^. responseBody)

instance (Monad m, MonadDiscourse m) => MonadDiscourse (ReaderT r m) where
    getLatest = lift getLatest
