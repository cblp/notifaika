-- |
-- Module: RSS
-- Author: Alexander Vershilov <alexander.vershilov@gmail.com>
--
--
module RSS
  ( Item(..)
  , MonadRss(..)
  , RssT
  , runRssT
  , initializeFeeds
  , updateFeeds
  , module RSS.Cache
  ) where

-- component
import RSS.Types
import RSS.Cache
import Gitter.Monad

-- global
import Network.Wreq
import Data.Foldable
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HM
import Text.XML.Lens
import Text.XML
import Control.Monad.Reader
import Control.Monad.Catch

-- | Basic RSS monad.
newtype RssT m a = RssT { _runRssT :: ReaderT RSS m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow)

-- | Run RSS action.
runRssT :: RSS -> RssT m a -> m a
runRssT cfg (RssT f) = runReaderT f cfg

-- | MTL class for RSS monad
class MonadRss m where
    -- | Load all configured feeds
    loadFeeds :: m [(FeedUrl, [Item])]

instance MonadIO m => MonadRss (RssT m) where
    loadFeeds = RssT . ReaderT $ \(RSS urls) ->
        foldrM (\url t -> do items <- loadFeed url
                             return $ (url, items):t) [] urls

-- Instances
instance (Monad m, MonadRss m) => MonadRss (ReaderT r m) where
   loadFeeds = lift loadFeeds

deriving instance MonadGitter m => MonadGitter (RssT m)

-- | Load concrete feed
loadFeed :: MonadIO m => FeedUrl -> m [Item]
loadFeed url = do
    r <- liftIO $ get (Text.unpack $ getFeedUrl url)
    Right xml <- return . parseLBS def $ r ^. responseBody
    return (extractItems xml)

-- | Get feed items out of the document.
extractItems :: Document -> [Item]
extractItems xml = concat $
      (\x -> channelItem (channelTitle x) x)
          <$> xml ^.. root ./ el "channel"
  where
    channelTitle x = x ^. el "channel" ./ el "title" . text
    channelItem t x = (Item <$> itemTitle <*> itemLink <*> pure t)
      <$> x ^.. el "channel" ./ el "item"
    itemTitle x = x ^. el "item" ./ el "title" . text
    itemLink x  = x ^. el "item" ./ el "link" . text

-- | Update cache, if feed does not exist in it, then add that feed.
initializeFeeds :: RSSCache -> [(FeedUrl,[Item])] -> RSSCache
initializeFeeds = foldl' initialize
  where
    initialize c@(RSSCache h) (u,l) = case HM.lookup u h of
        Nothing -> RSSCache (HM.insert u (item_link <$> l) h)
        Just _  -> c

-- | Update cache, by instering all new feeds, returns new cache and
-- items that were added.
updateFeeds :: RSSCache -> [(FeedUrl, [Item])] -> (RSSCache, [Item])
updateFeeds z = foldl' (\(c,n) f -> updateFeed c f n) (z,[])
   where
     updateFeed (RSSCache s) (url, itms) n
         = (RSSCache (HM.insert url new s), added <> n)
       where
         old  = HM.lookupDefault [] url s
         sOld = Set.fromList old
         added  = filter ((`Set.notMember` sOld) . item_link) itms
         new = old <> (item_link <$> added)
