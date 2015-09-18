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
          effects <- execTestIO repostUpdates
          let effectsExpected = [DiscourseGetLatestJson]
          assertEqual "effects" effectsExpected effects
    ]
