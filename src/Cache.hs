module Cache where

-- component
import EventSource
-- global
import Control.Monad.Reader
import Control.Monad.Writer

class MonadCache m where
    load  :: EventSource -> m (Maybe [Eid])
    save  :: EventSource -> [Eid] -> m ()

instance (Monad m, MonadCache m, Monoid w) => MonadCache (WriterT w m) where
    load = lift . load
    save key = lift . save key

instance (Monad m, MonadCache m) => MonadCache (ReaderT r m) where
    load = lift . load
    save key = lift . save key
