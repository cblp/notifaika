{-# LANGUAGE DeriveFunctor #-}

module TestIO where

-- project
import Discourse
-- general
import Control.Monad.Logger
import Data.Monoid
import Data.Text

data TestIO a = TestIO { testIO_log :: [Text], testIO_value :: a }
    deriving Functor

instance Applicative TestIO where
    pure x = TestIO {testIO_log = [], testIO_value = x}
    tf <*> tx =
        let TestIO {testIO_log = logF, testIO_value = f} = tf
            TestIO {testIO_log = logX, testIO_value = x} = tx
        in  TestIO {testIO_log = logX <> logF, testIO_value = f x}

instance Monad TestIO where
    tx >>= f =
        let TestIO {testIO_log = logX, testIO_value = x} = tx
            TestIO {testIO_log = logFx, testIO_value = y} = f x
        in  TestIO {testIO_log = logX <> logFx, testIO_value = y}

instance MonadLogger TestIO where
    monadLoggerLog = error "not implemented MonadLogger TestIO.monadLoggerLog"

instance MonadDiscourse TestIO where
    getLatest = error "not implemented MonadDiscourse TestIO.getLatest"
