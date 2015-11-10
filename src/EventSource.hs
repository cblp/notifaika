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
import RSS
import Types
-- global
import Control.Monad.Reader
import Control.Monad.Writer

data EventSource = Discourse Url | RSS Url
    deriving (Eq, Ord, Show)

class MonadEventSource m where
    getEvents :: EventSource -> m [Event]

instance MonadEventSource IO where
    getEvents (Discourse baseUrl) = getDiscourseEvents baseUrl
    getEvents (RSS feedUrl) = getRssEvents feedUrl

instance (Monad m, MonadEventSource m) => MonadEventSource (ReaderT r m) where
    getEvents = lift . getEvents

instance (Monoid w, Monad m, MonadEventSource m) =>
    MonadEventSource (WriterT w m) where
        getEvents = lift . getEvents
