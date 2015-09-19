module Lib where

import Cache
import Discourse

import Control.Monad.Logger
import Data.Text as Text

repostUpdates :: (MonadCache [Post] m, MonadDiscourse m, MonadLogger m) => m ()
repostUpdates = do
    latestPosts <- Discourse.getLatest
    $logDebug (showText latestPosts)
    cachedPosts <- loadDef []
    $logDebug (showText (latestPosts == cachedPosts))
    return ()

showText :: Show a => a -> Text
showText = Text.pack . show
