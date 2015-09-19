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
          TestIOResult{..} <- execTestIO repostUpdates
          let effectsExpected = [ DiscourseGet "/latest.json"
                                , CacheRead
                                , CacheWrite
                                , GitterPost
                                ]
          assertEqual "effects" effectsExpected testIOResult_effects
          assertEqual "cache" [] testIOResult_cache
    ]
