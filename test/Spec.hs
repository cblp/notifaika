import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup ""
    [ testCase "ok" $
          return ()
    ]
