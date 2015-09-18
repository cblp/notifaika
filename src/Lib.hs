{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Discourse

import Control.Monad.Logger
import Data.Text as Text

repostUpdates :: (MonadDiscourse m, MonadLogger m) => m ()
repostUpdates = do
    latestPosts <- Discourse.getLatest
    $logDebug $ showText latestPosts
    return ()

showText :: Show a => a -> Text
showText = Text.pack . show
