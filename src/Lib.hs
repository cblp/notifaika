module Lib where

-- component
import Cache
import Discourse
-- general
import            Control.Monad.Logger
import            Data.Monoid
import            Data.Text ( Text )
import qualified  Data.Text as Text

detectNewTopics :: [Topic] -> [Topic] -> [Topic]
detectNewTopics []   =
    return . maximum
detectNewTopics olds =
    filter $ \topic ->
        any (\old -> topic_id old /= topic_id topic && old <= topic) olds

repostUpdates :: (MonadCache [Topic] m, MonadDiscourse m, MonadLogger m) => m ()
repostUpdates = do
    latestTopics <- Discourse.getLatest
    cachedTopics <- loadDef []
    let newTopics = detectNewTopics cachedTopics latestTopics
    $logDebug ("newTopics = " <> showText newTopics)
    save latestTopics
    return ()

showText :: Show a => a -> Text
showText = Text.pack . show
