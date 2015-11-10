{-
    Discourse-to-Gitter reposts notification
    from Discourse forums to Gitter chats.
    Copyright (C) 2015 Yuriy Syrovetskiy

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

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
