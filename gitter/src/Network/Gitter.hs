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

module Network.Gitter
    ( Gitter(..)
    , GitterT
    , runGitterT
    , sendChatMessage
    , withRoom
    ) where

import Network.Gitter.Monad
import Network.Gitter.Types

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
    deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadTrans)

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
    lift $ runGitterAction (["rooms", roomId] <> path) request

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
        response <- liftIO (postWith opts url apiRequest)
        jsonResponse <- asJSON response
        return (jsonResponse ^. responseBody)
      where
        normalizeSpace = ByteString.unwords . ByteString.words
