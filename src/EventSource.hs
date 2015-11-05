module EventSource where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Aeson.TH
import Data.String.X
import Data.Text    as Text
import Database.Persist.Sql
import Network.Wreq as Wreq

type Url = String

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

-- | Event id, unique inside a feed
newtype Eid = Eid Text
    deriving (Eq, Ord, PersistField, PersistFieldSql, Show)

topic_eid :: Topic -> Eid
topic_eid = Eid . Text.pack . show . topic_id

data EventSource = Discourse Url
    deriving (Eq, Ord, Show)
deriveFromJSON defaultOptions ''EventSource

class MonadEventSource m where
    getTopics :: EventSource -> m [Topic]

instance MonadEventSource IO where
    getTopics (Discourse baseUrl) = do
        response <- liftIO $ Wreq.get (baseUrl <> "/latest.json")
        jsonResponse <- asJSON response
        return (jsonResponse ^. responseBody)

instance (Monad m, MonadEventSource m) => MonadEventSource (ReaderT r m) where
    getTopics = lift . getTopics

instance (Monoid w, Monad m, MonadEventSource m) => MonadEventSource (WriterT w m) where
    getTopics = lift . getTopics
