{-# LANGUAGE NamedFieldPuns #-}

-- component
import TestIO
-- project
import Lib
-- general
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup ""
    [ testCase "repostUpdates" $ do
          let TestIO{testIO_log, testIO_value = ()} = repostUpdates
          assertEqual "log" testIO_log []
    ]
