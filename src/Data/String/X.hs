module Data.String.X where

-- |  Very primitive way to strip prefix.
--    To use in deriveJSON's fieldLabelModifier.
dropPrefix :: String -> String -> String
dropPrefix = drop . length
