module Data.Aeson.X (module Data.Aeson, module Data.Aeson.X) where

import Data.Aeson
import Data.ByteString.Lazy as ByteString
import Data.Monoid

decodeFile :: FromJSON a => FilePath -> IO a
decodeFile filepath = do
    bytes <- ByteString.readFile filepath
    let decodeResult = eitherDecode bytes
    case decodeResult of
        Left decodeError ->
            error ("Cannot decode file \"" <> filepath <> "\": " <> decodeError)
        Right value ->
            return value
