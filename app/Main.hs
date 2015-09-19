module Main where

import Cache
import Lib

import Control.Monad.Logger

main :: IO ()
main = run repostUpdates "/var/cache/discourse-to-gitter/latest"
  where
    run action cacheFile = runStderrLoggingT (runFileCacheT action cacheFile)
