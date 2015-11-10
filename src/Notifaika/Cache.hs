{-
    Notifaika reposts notifications
    from different feeds to Gitter chats.
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

module Notifaika.Cache where

import Notifaika.EventSource
import Notifaika.Types

import Control.Monad.Reader
import Control.Monad.Writer

class MonadCache m where
    load :: EventSource -> m (Maybe [Eid])
    save :: EventSource -> [Eid] -> m ()

instance (Monad m, MonadCache m, Monoid w) => MonadCache (WriterT w m) where
    load = lift . load
    save key = lift . save key

instance (Monad m, MonadCache m) => MonadCache (ReaderT r m) where
    load = lift . load
    save key = lift . save key
