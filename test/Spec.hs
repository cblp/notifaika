{-# LANGUAGE DeriveFunctor #-}

import Lib

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup ""
    [ testCase "repostUpdates" $
          runTestIO repostUpdates
    ]

data TestIO a = TestIO a deriving Functor
instance Applicative TestIO where
    pure = TestIO
    (TestIO f) <*> tio = fmap f tio
instance Monad TestIO where
    (TestIO x) >>= f = f x

runTestIO :: TestIO a -> IO a
runTestIO (TestIO x) = return x
