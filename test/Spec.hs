{-# LANGUAGE NamedFieldPuns #-}

-- component
import TestIO
-- package
import Discourse
import Lib
-- general
import Data.Time.Clock.POSIX
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup ""
    [ testCase "detectNewPosts" $ do
          assertEqual "if no old, select the 1 newest"
              [post 3 3]
              (detectNewPosts [] [post 1 1, post 3 3, post 2 2])
          assertEqual "if some old, select only newer"
              [post 3 3]
              (detectNewPosts [post 2 2] [post 1 1, post 3 3, post 2 2])
          assertEqual "if time same, check ids"
              [post 20 2, post 22 2]
              (detectNewPosts [post 21 2] [post 20 2, post 21 2, post 22 2])
    , testCase "repostUpdates" $ do
          TestIOResult{..} <- execTestIO repostUpdates
          let effectsExpected = [ DiscourseGet "/latest.json"
                                , CacheRead
                                , CacheWrite
                                -- TODO , GitterPost
                                ]
          assertEqual "effects" effectsExpected testIOResult_effects
          -- TODO assertEqual "cache" [] testIOResult_cache
    ]

post :: Integer -> Integer -> Post
post pid time =
    Post  { post_id = pid
          , post_created_at = posixSecondsToUTCTime (fromIntegral time)
          }
