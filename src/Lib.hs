module Lib where

-- component
import Cache
import Discourse
-- general
import            Control.Monad.Logger
import            Data.Monoid
import            Data.Text ( Text )
import qualified  Data.Text as Text

detectNewPosts :: [Post] -> [Post] -> [Post]
detectNewPosts []   =
    return . maximum
detectNewPosts olds =
    filter $ \post ->
        any (\old -> post_id old /= post_id post && old <= post) olds

repostUpdates :: (MonadCache [Post] m, MonadDiscourse m, MonadLogger m) => m ()
repostUpdates = do
    latestPosts <- Discourse.getLatest
    cachedPosts <- loadDef []
    let newPosts = detectNewPosts cachedPosts latestPosts
    $logDebug ("newPosts = " <> showText newPosts)
    save latestPosts
    return ()

showText :: Show a => a -> Text
showText = Text.pack . show
