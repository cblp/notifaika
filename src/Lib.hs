module Lib where

-- component
import Cache
import Discourse
-- general
import            Control.Monad.Logger
import            Data.List
import            Data.Monoid
import            Data.Ord
import            Data.Text ( Text )
import qualified  Data.Text as Text

detectNewPosts :: [Post] -> [Post] -> [Post]
detectNewPosts []   = take 1 . sortBy (comparing Down)
detectNewPosts olds = filter (\post -> any (< post) olds)

repostUpdates :: (MonadCache [Post] m, MonadDiscourse m, MonadLogger m) => m ()
repostUpdates = do
    latestPosts <- Discourse.getLatest
    cachedPosts <- loadDef []
    let newPosts = detectNewPosts cachedPosts latestPosts
    $logDebug ("newPosts = " <> showText newPosts)
    return ()

showText :: Show a => a -> Text
showText = Text.pack . show
