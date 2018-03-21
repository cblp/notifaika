{-
    Notifaika reposts notifications
    from different feeds to Gitter chats.
    Copyright (C) 2015  Alexander Vershilov <alexander.vershilov@gmail.com>,
                        Yuriy Syrovetskiy <cblp@cblp.su>
                  2016  Yuriy Syrovetskiy <cblp@cblp.su>

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

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Notifaika.RSS
    ( getRssEvents
    , RSSException
    ) where

import           Control.Exception (Exception, throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Lens ((^.))
import           Data.Maybe (mapMaybe)
import           Network.Wreq (get, responseBody)
import           Text.Feed.Import (parseFeedSource)
import           Text.Feed.Query (getFeedItems, getFeedTitle,
                                  getItemLink, getItemTitle)
import           Text.Feed.Types (Feed)

import Notifaika.Types (Eid (Eid), Event (Event), Url, eventId, message)

data RSSException
    = FeedParsingError
    deriving (Eq, Show)

instance Exception RSSException

-- | Load concrete feed
getRssEvents :: MonadIO m => Url -> m [Event]
getRssEvents url = do
    r <- liftIO $ get url
    case parseFeedSource (r ^. responseBody) of
        Nothing ->
            liftIO $ throwIO FeedParsingError
        Just feed ->
            pure $ extractEvents feed

extractEvents :: Feed -> [Event]
extractEvents feed = mapMaybe extract (getFeedItems feed)
  where
    extract item = do
        link <- getItemLink item
        title <- getItemTitle item
        pure Event
            { eventId = Eid link
            , message = mconcat
                [ "Новый пост «", title
                , "» в ленте «", getFeedTitle feed, "»\n"
                , link
                ]
            }
