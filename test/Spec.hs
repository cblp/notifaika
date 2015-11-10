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

import TestIO

import Notifaika.Core
import Notifaika.EventSource
import Notifaika.RSS

import Data.Map as Map
import Data.Text
import Test.Tasty
import Test.Tasty.HUnit
import Text.XML

main :: IO ()
main = defaultMain $ testGroup ""
    [ test_extractItems
    , test_repostUpdates
    ]

test_extractItems :: TestTree
test_extractItems = testGroup "extractItems"
  [ testCase "ok" $ do
      let xml = Document
            { documentPrologue = undefined
            , documentRoot = element' "rss"
              [ element "channel"
                [ element "title" [content "c"]
                , element "item"
                  [element "title" [content "a"], element "link" [content "b"]]
                ]
              ]
            , documentEpilogue = undefined
            }
          items = extractItems xml
          itemsExpected =
            [Item{item_channel = "c", item_link = "b", item_title = "a"}]
      assertEqual "items" itemsExpected items
  ]

test_repostUpdates :: TestTree
test_repostUpdates = testGroup "repostUpdates"
    [ testCase "no new events" $ do
          let initCache = Map.fromList
                  [Discourse "test://discourse-no-data.example.com" -: Just []]
          TestIOResult{..} <- execTestIO initCache repostUpdates
          let effectsExpected =
                  [ EventsGet
                  , CacheRead
                  ]
          assertEqual "effects" effectsExpected testIOResult_effects
    , testCase "not cached" $ do
          let initCache = Map.fromList
                  [Discourse "test://discourse-no-data.example.com" -: Nothing]
          TestIOResult{..} <- execTestIO initCache repostUpdates
          let effectsExpected =
                  [ EventsGet
                  , CacheRead
                  , CacheWrite
                  ]
          assertEqual "effects" effectsExpected testIOResult_effects
    , testCase "some new events" $ do
          let initCache = Map.fromList
                  [Discourse "test://discourse.example.com" -: Just []]
          TestIOResult{..} <- execTestIO initCache repostUpdates
          let effectsExpected =
                  [ EventsGet
                  , CacheRead
                  , GitterAction ["rooms"]
                  , GitterAction ["rooms", "exampleroomid", "chatMessages"]
                  , GitterAction ["rooms"]
                  , GitterAction ["rooms", "exampleroomid", "chatMessages"]
                  , CacheWrite
                  ]
          assertEqual "effects" effectsExpected testIOResult_effects
    ]

-- topic :: Integer -> Integer -> Topic
-- topic tid time =
--     Topic { topic_created_at = posixSecondsToUTCTime (fromIntegral time)
--           , topic_fancy_title = "example title"
--           , topic_id = tid
--           , topic_posters = []
--           , topic_slug = "example"
--           }

(-:) :: a -> b -> (a, b)
(-:) = (,)

element :: Name -> [Node] -> Node
element name = NodeElement . element' name

element' :: Name -> [Node] -> Element
element' name nodes = Element
    {elementName = name, elementAttributes = undefined, elementNodes = nodes}

content :: Text -> Node
content = NodeContent
