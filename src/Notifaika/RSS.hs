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

module Notifaika.RSS
    ( Item(..)
    , extractItems
    , getRssEvents
    ) where

import Notifaika.RSS.Types
import Notifaika.Types

import Data.Monoid
import Network.Wreq
import Text.XML.Lens ( (^.), (^..), attr, el, plate, root, text )
import Text.XML
import Control.Monad.Reader

-- | Load concrete feed
getRssEvents :: MonadIO m => Url -> m [Event]
getRssEvents url = do
    r <- liftIO $ get url
    Right xml <- return . parseLBS def $ r ^. responseBody
    return  [ Event{eventId = Eid item_link, message}
            | Item{item_title, item_channel, item_link} <- extractItems xml
            , let message = mconcat
                      [ "Новый пост «", item_title
                      , "» в ленте «", item_channel, "»\n"
                      , item_link
                      ]
            ]

-- | Get feed items out of the document.
extractItems :: Document -> [Item]
extractItems doc = do
    elChannel <- doc ^.. root . channel
    let item_channel = elChannel ^. title . text
    elItem <- elChannel ^.. item
    pure Item { item_channel
              , item_link = elItem ^. link
              , item_title = elItem ^. title . text
              }
  where
    channel = child "channel" <> el (atom "feed")
    title = child "title" <> child (atom "title")
    item = child "item" <> child (atom "entry")
    link = child "link" . text <> child (atom "link") . attr "href"
    atom name = Name  { nameLocalName = name
                      , nameNamespace = Just "http://www.w3.org/2005/Atom"
                      , namePrefix = Nothing
                      }
    child elname = plate . el elname
