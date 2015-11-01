-- |
-- Module: RSS.Types
-- Author: Alexander Vershilov <alexander.vershilov@gmail.com>
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RSS.Types
  ( RSS(..)
  , FeedUrl(..)
  , Item(..)
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.String.X
import Data.Hashable

-- | URL of the rss feed.
newtype FeedUrl = FeedUrl { getFeedUrl :: Text }
    deriving (Eq,Show,ToJSON, FromJSON, Hashable)

-- | RSS configuraion.
data RSS = RSS
  { rss_feeds   :: [FeedUrl] -- ^ List of RSS Feeds.
  } deriving (Show)

deriveFromJSON  defaultOptions { fieldLabelModifier = dropPrefix "rss_" }
                ''RSS

-- | RSS Item
data Item = Item
  { item_title   :: Text
  , item_link    :: Text
  , item_channel :: Text
  } deriving (Eq,Show)

