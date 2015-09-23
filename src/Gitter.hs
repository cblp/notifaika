module Gitter
    ( Gitter(..)
    , GitterT
    , runGitterT
    , sendChatMessage
    , withRoom
    ) where

-- component
import Cache
import Discourse
import Gitter.Monad
import Gitter.Types
-- global
import            Control.Error
import            Control.Lens
import            Control.Monad.Catch
import            Control.Monad.Reader
import            Data.Aeson
import            Data.Aeson.Lens
import            Data.ByteString.Char8 as ByteString
import            Data.List as List
import            Data.Monoid
import            Data.Text ( Text )
import qualified  Data.Text as Text
import            Network.Wreq

newtype GitterT m a = GitterT (ReaderT Gitter m a)
    deriving (Applicative, Functor, Monad, MonadDiscourse, MonadIO)

runGitterT :: Gitter -> GitterT m a -> m a
runGitterT gitter (GitterT readerAction) = runReaderT readerAction gitter

newtype GitterRoomT m a = GitterRoomT (ReaderT Room m a)
    deriving Functor

roomUri :: Room -> RoomUri
roomUri (ONETOONE user) = user
roomUri (REPO user repo) = user <> "/" <> repo

withRoom :: MonadGitter gitter => Room -> GitterRoomT gitter a -> gitter a
withRoom room (GitterRoomT readerAction) =
    runReaderT readerAction room

sendChatMessage :: MonadGitter m => Text -> GitterRoomT m ()
sendChatMessage text =
    void . runRoomAction ["chatMessages"] $ object [("text", String text)]

runRoomAction :: MonadGitter m => ResourcePath -> Value -> GitterRoomT m Value
runRoomAction path request = GitterRoomT $ do
    room <- ask
    roomId <- lift $ joinRoom room
    lift $ runGitterAction (["room", roomId] <> path) request

joinRoom :: MonadGitter m => Room -> m RoomId
joinRoom room = do
    jsonResponse <- runGitterAction ["rooms"] $
        object [("uri", String $ roomUri room)]
    maybe (fail "joining room must return a string \"id\"") return $
        jsonResponse ^? key "id" . _String

instance (MonadIO io, MonadThrow io) => MonadGitter (GitterT io) where
    runGitterAction path apiRequest = GitterT $ do
        Gitter{..} <- ask
        tokenFileContents <- liftIO $ ByteString.readFile gitter_tokenFile
        let token = normalizeSpace tokenFileContents
            url = List.intercalate "/" (gitter_baseUrl : fmap Text.unpack path)
            opts = defaults &~ auth ?= oauth2Bearer token
        liftIO (errLn ("url = " <> url))
        liftIO (errLn ("opts = " <> show opts))
        response <- liftIO (postWith opts url apiRequest)
        jsonResponse <- asJSON response
        return (jsonResponse ^. responseBody)
      where
        normalizeSpace = ByteString.unwords . ByteString.words

instance MonadTrans GitterT where
    lift = GitterT . lift

instance (Monad m, MonadCache a m) => MonadCache a (GitterT m) where
    loadDef = lift . loadDef
    save = lift . save
