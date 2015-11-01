-- |
-- Module: RSS.Cache
-- Author: Alexander Vershilov <alexander.vershilov@gmail.com>
--
module RSS.Cache
  ( RSSCache(..)
  , emptyCache
  ) where

-- component
import RSS.Types

-- global
import Control.Arrow
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.Text (Text)

-- | Cache of the loaded links.
newtype RSSCache = RSSCache (HashMap FeedUrl [Text]) deriving (Show)

emptyCache :: RSSCache
emptyCache = RSSCache HM.empty

-- Manua instances reuse internal aeson structure

instance ToJSON RSSCache where
  toJSON (RSSCache s) = object $ (getFeedUrl *** toJSON) <$> HM.toList  s

instance FromJSON RSSCache where
  parseJSON = withObject "rss-cache" $ \s -> do
    let w = (\(k,v) -> (,) <$> pure (FeedUrl k) <*> parseJSON v) <$> HM.toList s
    t <- sequence w
    return . RSSCache $ HM.fromList t
