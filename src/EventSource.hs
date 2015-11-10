module EventSource where

-- component
import Discourse
import Types
-- global
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Aeson.TH

data EventSource = Discourse Url
    deriving (Eq, Ord, Show)
deriveFromJSON defaultOptions ''EventSource

class MonadEventSource m where
    getTopics :: EventSource -> m [Discourse.Topic]

instance MonadEventSource IO where
    getTopics (Discourse baseUrl) = getDiscourseTopics baseUrl

instance (Monad m, MonadEventSource m) => MonadEventSource (ReaderT r m) where
    getTopics = lift . getTopics

instance (Monoid w, Monad m, MonadEventSource m) => MonadEventSource (WriterT w m) where
    getTopics = lift . getTopics
