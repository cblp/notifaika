module Main where

import Lib

import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT repostUpdates
