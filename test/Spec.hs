{-# LANGUAGE NamedFieldPuns #-}

-- component
import TestIO
-- package
import Lib
-- general
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup ""
    [ testCase "repostUpdates" $ do
          worldChanges <- execTestIO repostUpdates
          assertEqual "worldChanges" worldChanges []
    ]
