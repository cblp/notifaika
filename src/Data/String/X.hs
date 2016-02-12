module Data.String.X where

dropPrefix :: String -> String -> String
dropPrefix = drop . length
